
{-# LANGUAGE DataKinds #-} --Makes the kind system extensible
{-# LANGUAGE TemplateHaskell #-} --allows you to do type-safe compile-time meta-programming
{-# LANGUAGE NoImplicitPrelude   #-} --PlutusTx prelude has priority over Haskell prelude
{-# LANGUAGE ScopedTypeVariables #-}

{-# LANGUAGE TypeApplications    #-} --required to use custo datatypes
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE TypeOperators       #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE RecordWildCards #-} -- To allow notation like GrabParams {..}

{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}

module Beneficiary.BeneficiaryOffChain where

-- Haskell imports
import qualified Control.Monad            as Monad (void)
import qualified GHC.Generics                        as GHCGenerics (Generic)
import qualified Data.Aeson                          as DataAeson (ToJSON, FromJSON)
import qualified Data.OpenApi.Schema                 as DataOpenApiSchema (ToSchema)
import qualified Prelude                  as P
import qualified Data.Void                as Void (Void)
import qualified Data.Map                 as Map
import qualified Data.Text                           as DataText (Text)

-- Plutus imports
import qualified PlutusTx
import PlutusTx.Prelude
import qualified Plutus.Contract          as PlutusContract
import qualified Ledger.Ada               as Ada
import qualified Ledger.Tx                as LedgerTx
import qualified Plutus.V2.Ledger.Api                            as LedgerApiV2
import qualified Ledger                                          (PaymentPubKeyHash)
import qualified Text.Printf              as TextPrintf (printf)
import qualified Ledger.Constraints       as Constraints
import qualified Plutus.V1.Ledger.Scripts as ScriptsLedger

import qualified Beneficiary.BeneficiaryOnChain as OnChain
-- 1. Create utxos in the script address

--endpoints (contract monads)

data StartParams = StartParams 
    {
        spBeneficiary :: !Ledger.PaymentPubKeyHash,
        spDeadline :: !LedgerApiV2.POSIXTime,
        spGuess :: !Integer,
        spAmount :: !Integer
    } deriving (GHCGenerics.Generic, DataAeson.ToJSON, DataAeson.FromJSON, DataOpenApiSchema.ToSchema)

data GrabParams = GrabParams
    {
        grabRedeem :: !Integer
    } deriving (GHCGenerics.Generic, DataAeson.ToJSON, DataAeson.FromJSON, DataOpenApiSchema.ToSchema)


type GiftSchema = 
    PlutusContract.Endpoint "start" StartParams
    PlutusContract..\/ PlutusContract.Endpoint "grab" GrabParams

start :: PlutusContract.AsContractError e => StartParams -> PlutusContract.Contract w s e ()
start sp = do
    let d = OnChain.Dat
            {
                OnChain.beneficiary = spBeneficiary sp,
                OnChain.deadline = spDeadline sp,
                OnChain.ddata = spGuess sp
            }
        v = Ada.lovelaceValueOf $ spAmount sp
    -- PlutusContract.logInfo @P.String $ TextPrintf.printf "Start of the give action"
        tx = Constraints.mustPayToOtherScript OnChain.validatorHash (ScriptsLedger.Datum $ PlutusTx.toBuiltinData d) v
        lookups = Constraints.plutusV2OtherScript OnChain.validator
    submittedTx <- PlutusContract.submitTxConstraintsWith @Void.Void lookups tx
    Monad.void $ PlutusContract.awaitTxConfirmed $ LedgerTx.getCardanoTxId submittedTx
    -- PlutusContract.logInfo @P.String $ TextPrintf.printf "Made transaction of %d adas" q

grab :: GrabParams -> PlutusContract.Contract w s DataText.Text ()
grab GrabParams{..} = do
    pkh <- PlutusContract.ownFirstPaymentPubKeyHash
    now <- PlutusContract.currentTime
    maybeutxo <- findUtxoInValidator pkh grabRedeem now
    case maybeutxo of
        Nothing -> PlutusContract.logInfo @P.String $ TextPrintf.printf "Wrong guess %d" grabRedeem
        Just (oref, o) -> do
            PlutusContract.logInfo @P.String $ TextPrintf.printf "Redeem utxos %s" (P.show oref)
            let 
                r = OnChain.Redeem 
                    {
                        OnChain.redeem = grabRedeem
                    }
                lookups = Constraints.unspentOutputs (Map.singleton oref o) P.<>
                          Constraints.plutusV2OtherScript OnChain.validator
                tx = Constraints.mustSpendScriptOutput oref (ScriptsLedger.Redeemer $ PlutusTx.toBuiltinData r) P.<>
                     Constraints.mustValidateIn (LedgerApiV2.from now)
                     
            submittedTx <- PlutusContract.submitTxConstraintsWith @OnChain.Simple lookups tx
            Monad.void $ PlutusContract.awaitTxConfirmed $ LedgerTx.getCardanoTxId submittedTx
            PlutusContract.logInfo @P.String $ "collected gifts"

getDatum :: (LedgerApiV2.TxOutRef, LedgerTx.ChainIndexTxOut) -> Maybe OnChain.Dat
getDatum (_, o) = do
    let datHashOrDatum = LedgerTx._ciTxOutScriptDatum o
    LedgerApiV2.Datum e <- snd datHashOrDatum
    case (LedgerApiV2.fromBuiltinData e :: Maybe OnChain.Dat) of
        Nothing -> Nothing
        d -> d

checkUTXO :: (LedgerApiV2.TxOutRef, LedgerTx.ChainIndexTxOut) -> Ledger.PaymentPubKeyHash -> Integer -> LedgerApiV2.POSIXTime -> Bool
checkUTXO (oref, o) pkh n now = do
    case getDatum (oref, o) of
        Nothing -> False 
        Just OnChain.Dat{..}
            | ddata == n && beneficiary == pkh && now >= deadline -> True 
            | otherwise -> False

findUTXO :: [(LedgerApiV2.TxOutRef, LedgerTx.ChainIndexTxOut)] -> Ledger.PaymentPubKeyHash -> Integer -> LedgerApiV2.POSIXTime -> (Maybe (LedgerApiV2.TxOutRef, LedgerTx.ChainIndexTxOut))
findUTXO [] _ _ _ = Nothing
findUTXO [(oref, o)] pkh n now = do
    if checkUTXO (oref, o) pkh n now then
        return (oref, o)
    else 
        Nothing
findUTXO ((oref, o):xs) pkh n now
    | checkUTXO (oref, o) pkh n now = return (oref, o)
    | otherwise = findUTXO xs pkh n now

findUtxoInValidator :: Ledger.PaymentPubKeyHash -> Integer -> LedgerApiV2.POSIXTime -> PlutusContract.Contract w s DataText.Text (Maybe (LedgerApiV2.TxOutRef, LedgerTx.ChainIndexTxOut))
findUtxoInValidator pkh n now = do
    utxos <- PlutusContract.utxosAt OnChain.address
    let xs = [(oref, o) | (oref, o) <- Map.toList utxos]
        out = findUTXO xs pkh n now
    return out

endpoints :: PlutusContract.Contract () GiftSchema DataText.Text ()
endpoints = PlutusContract.awaitPromise (start' `PlutusContract.select` grab') >> endpoints
    where 
        start' = PlutusContract.endpoint @"start" start
        grab' = PlutusContract.endpoint @"grab" $ grab
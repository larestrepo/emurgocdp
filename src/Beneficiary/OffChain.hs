
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE ScopedTypeVariables #-}

{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE TypeOperators       #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE RecordWildCards #-}


module Beneficiary.OffChain where

-- Haskell imports
import qualified Control.Monad            as Monad (void)
import qualified GHC.Generics                        as GHCGenerics (Generic)
import qualified Data.Aeson                          as DataAeson (ToJSON, FromJSON)
import qualified Data.OpenApi.Schema                 as DataOpenApiSchema (ToSchema)
import qualified Prelude                  as P
import qualified Data.Map                 as Map
import qualified Data.Text                           as DataText (Text)

-- Plutus imports

import qualified Plutus.Contract          as PlutusContract
import qualified Text.Printf              as TextPrintf (printf)
import           PlutusTx.Prelude
import           Ledger.Ada               as Ada
import qualified Ledger.Constraints       as Constraints
import qualified Ledger.Tx                as LedgerTx
-- import qualified Data.Void                as Void (Void)
import qualified PlutusTx
import qualified Plutus.V1.Ledger.Scripts as ScriptsLedger
import qualified Ledger  
import qualified Plutus.V2.Ledger.Api                            as LedgerApiV2


import qualified Beneficiary.OnChain as OnChain

data ProduceParams = ProduceParams
    {
        
        ppBeneficiary :: Ledger.PaymentPubKeyHash
        , ppDeadline :: LedgerApiV2.POSIXTime
        , ppGuess :: Integer
        , ppAmount :: Integer
    } deriving (GHCGenerics.Generic, DataAeson.ToJSON, DataAeson.FromJSON, DataOpenApiSchema.ToSchema)

data ConsumeParams = ConsumeParams
    {
        getRedeem :: Integer
    } deriving (GHCGenerics.Generic, DataAeson.ToJSON, DataAeson.FromJSON, DataOpenApiSchema.ToSchema)


type BenefTypeSchema = 
    PlutusContract.Endpoint "produce" ProduceParams 
    PlutusContract..\/ PlutusContract.Endpoint "consume" ConsumeParams

produce :: forall w s e. PlutusContract.AsContractError e => ProduceParams -> PlutusContract.Contract w s e ()
produce pp = do 
    let d = OnChain.Beneficiary
            {
            OnChain.beneficiary = ppBeneficiary pp
            , OnChain.deadline = ppDeadline pp
            , OnChain.guess = ppGuess pp
            }
        value = ppAmount pp
    PlutusContract.logInfo @P.String "------------------------produce start initialize------------------------"
    let tx = Constraints.mustPayToOtherScript (OnChain.simpleHash) (ScriptsLedger.Datum $ PlutusTx.toBuiltinData d) (lovelaceValueOf value)
        lookups = Constraints.plutusV2OtherScript OnChain.validator

    submittedTx <- PlutusContract.submitTxConstraintsWith @OnChain.Benef lookups tx
    Monad.void $ PlutusContract.awaitTxConfirmed $ LedgerTx.getCardanoTxId submittedTx
    PlutusContract.logInfo @P.String "------------------------Transaction processed------------------------"
        

consume :: forall w s. ConsumeParams -> PlutusContract.Contract w s DataText.Text ()
consume ConsumeParams{..} = do 

    pkh <- PlutusContract.ownFirstPaymentPubKeyHash
    now <- PlutusContract.currentTime
    maybeutxo <- findUtxoInValidator pkh getRedeem now
    case maybeutxo of 
        Nothing -> PlutusContract.logInfo @P.String $ TextPrintf.printf "Not possible to retrieve"
        Just (oref, o) -> do
            PlutusContract.logInfo @P.String $ TextPrintf.printf "Redeem utxos %s - with timing now at %s:" (P.show oref) (P.show $ now)
            let r = OnChain.Redeem{ OnChain.redeem = getRedeem }
                lookups = Constraints.unspentOutputs (Map.singleton oref o) P.<>
                          Constraints.plutusV2OtherScript OnChain.validator
                tx = Constraints.mustSpendScriptOutput oref (ScriptsLedger.Redeemer $ PlutusTx.toBuiltinData r) P.<>
                     Constraints.mustValidateIn (LedgerApiV2.from now)
            submittedTx <- PlutusContract.submitTxConstraintsWith @OnChain.Benef lookups tx
            Monad.void $ PlutusContract.awaitTxConfirmed $ LedgerTx.getCardanoTxId submittedTx

--Utils file functions
getDatum :: (LedgerApiV2.TxOutRef, LedgerTx.ChainIndexTxOut) -> Maybe OnChain.Beneficiary
getDatum (_, o) = do
    let 
        datHashOrDatum = LedgerTx._ciTxOutScriptDatum o

    LedgerApiV2.Datum e <- snd datHashOrDatum
    
    case (LedgerApiV2.fromBuiltinData e :: Maybe OnChain.Beneficiary) of    
        Nothing -> Nothing
        d -> d

checkUTXO :: (LedgerApiV2.TxOutRef, LedgerTx.ChainIndexTxOut) -> Ledger.PaymentPubKeyHash ->Integer -> LedgerApiV2.POSIXTime -> Bool
checkUTXO (oref,o) ppkh n now = do
    case getDatum (oref,o) of
        Nothing -> False
        Just OnChain.Beneficiary{..}
            | beneficiary == ppkh && guess == n && now >= deadline -> True
            | otherwise                                            -> False

findUTXO :: [(LedgerApiV2.TxOutRef, LedgerTx.ChainIndexTxOut)] -> Ledger.PaymentPubKeyHash -> Integer -> LedgerApiV2.POSIXTime -> (Maybe (LedgerApiV2.TxOutRef, LedgerTx.ChainIndexTxOut))
findUTXO [] _ _ _ = Nothing
findUTXO [(oref,o)] ppkh n now  = do
    if checkUTXO (oref, o) ppkh n now then 
        return (oref,o)
    else 
        Nothing
findUTXO ((oref,o):xs) ppkh n now
    | checkUTXO (oref , o)  ppkh n now = return (oref,o)
    | otherwise = findUTXO xs ppkh n now

findUtxoInValidator :: Ledger.PaymentPubKeyHash -> Integer -> LedgerApiV2.POSIXTime -> PlutusContract.Contract w s DataText.Text (Maybe (LedgerApiV2.TxOutRef, LedgerTx.ChainIndexTxOut))
findUtxoInValidator ppkh n now = do
    utxos <- PlutusContract.utxosAt OnChain.simpleAddress
    let 
        xs = [ (oref, o) | (oref, o) <- Map.toList utxos ]
        out = findUTXO xs ppkh n now
    return out

endpoints :: PlutusContract.Contract () BenefTypeSchema DataText.Text ()
endpoints = PlutusContract.awaitPromise  (produce' `PlutusContract.select` consume') >> endpoints
    where 
        produce' = PlutusContract.endpoint @"produce" produce
        consume' = PlutusContract.endpoint @"consume" consume
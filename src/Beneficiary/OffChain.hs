
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
import qualified Control.Lens                                           as ControlLens

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
    let tx = Constraints.mustPayToOtherScriptWithDatumHash (OnChain.simpleHash) (ScriptsLedger.Datum $ PlutusTx.toBuiltinData d) (lovelaceValueOf value)
        lookups = Constraints.plutusV2OtherScript OnChain.validator

    submittedTx <- PlutusContract.submitTxConstraintsWith @OnChain.Benef lookups tx
    Monad.void $ PlutusContract.awaitTxConfirmed $ LedgerTx.getCardanoTxId submittedTx
    PlutusContract.logInfo @P.String "------------------------Transaction processed------------------------"
        

consume :: forall w s. ConsumeParams -> PlutusContract.Contract w s DataText.Text ()
consume ConsumeParams{..} = do 

    pkh <- PlutusContract.ownFirstPaymentPubKeyHash
    (_ , tconstraint) <- PlutusContract.currentNodeClientTimeRange
    maybeutxo <- findUtxoInValidator pkh getRedeem tconstraint
    case maybeutxo of 
        Nothing -> PlutusContract.logInfo @P.String $ TextPrintf.printf "Not possible to retrieve"
        Just (oref, o) -> do
            PlutusContract.logInfo @P.String $ TextPrintf.printf "Redeem utxos %s - with timing tconstraint at %s:" (P.show oref) (P.show $ tconstraint)
            let r = OnChain.Redeem{ OnChain.redeem = getRedeem }
                lookups = Constraints.unspentOutputs (Map.singleton oref o) P.<>
                          Constraints.plutusV2OtherScript OnChain.validator
                tx = Constraints.mustSpendScriptOutput oref (ScriptsLedger.Redeemer $ PlutusTx.toBuiltinData r) P.<>
                     Constraints.mustValidateIn (LedgerApiV2.from tconstraint)
            submittedTx <- PlutusContract.submitTxConstraintsWith @OnChain.Benef lookups tx
            Monad.void $ PlutusContract.awaitTxConfirmed $ LedgerTx.getCardanoTxId submittedTx

--Utils file functions
getDatum :: (LedgerApiV2.TxOutRef, LedgerTx.DecoratedTxOut) -> Maybe OnChain.Beneficiary
getDatum (_, o) = do
    (_, datumFromQuery) <- o ControlLens.^? LedgerTx.decoratedTxOutDatum
    LedgerApiV2.Datum e <- datumFromQuery ControlLens.^? LedgerTx.datumInDatumFromQuery

    case (LedgerApiV2.fromBuiltinData e :: Maybe OnChain.Beneficiary) of    
        Nothing -> Nothing
        d -> d

checkUTXO :: (LedgerApiV2.TxOutRef, LedgerTx.DecoratedTxOut) -> Ledger.PaymentPubKeyHash ->Integer -> LedgerApiV2.POSIXTime -> Bool
checkUTXO (oref,o) ppkh n tconstraint = do
    case getDatum (oref,o) of
        Nothing -> False
        Just OnChain.Beneficiary{..}
            | beneficiary == ppkh && guess == n && tconstraint >= deadline -> True
            | otherwise                                            -> False

findUTXO :: [(LedgerApiV2.TxOutRef, LedgerTx.DecoratedTxOut)] -> Ledger.PaymentPubKeyHash -> Integer -> LedgerApiV2.POSIXTime -> (Maybe (LedgerApiV2.TxOutRef, LedgerTx.DecoratedTxOut))
findUTXO [] _ _ _ = Nothing
findUTXO [(oref,o)] ppkh n tconstraint  = do
    if checkUTXO (oref, o) ppkh n tconstraint then 
        return (oref,o)
    else 
        Nothing
findUTXO ((oref,o):xs) ppkh n tconstraint
    | checkUTXO (oref , o)  ppkh n tconstraint = return (oref,o)
    | otherwise = findUTXO xs ppkh n tconstraint

findUtxoInValidator :: Ledger.PaymentPubKeyHash -> Integer -> LedgerApiV2.POSIXTime -> PlutusContract.Contract w s DataText.Text (Maybe (LedgerApiV2.TxOutRef, LedgerTx.DecoratedTxOut))
findUtxoInValidator ppkh n tconstraint = do
    utxos <- PlutusContract.utxosAt OnChain.simpleAddress
    let 
        xs = [ (oref, o) | (oref, o) <- Map.toList utxos ]
        out = findUTXO xs ppkh n tconstraint
    return out

endpoints :: PlutusContract.Contract () BenefTypeSchema DataText.Text ()
endpoints = PlutusContract.awaitPromise  (produce' `PlutusContract.select` consume') >> endpoints
    where 
        produce' = PlutusContract.endpoint @"produce" produce
        consume' = PlutusContract.endpoint @"consume" consume

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


module Parameterized.OffChain where

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


import qualified Parameterized.OnChain as OnChain

data ProduceParams = ProduceParams
    {
        ppCreator :: Ledger.PaymentPubKeyHash
        , ppBeneficiary :: Ledger.PaymentPubKeyHash
        , ppDeadline :: LedgerApiV2.POSIXTime
        , ppGuess :: Integer
        , ppAmount :: Integer
    } deriving (GHCGenerics.Generic, DataAeson.ToJSON, DataAeson.FromJSON, DataOpenApiSchema.ToSchema)

data ConsumeParams = ConsumeParams
    {
          gpCreator :: Ledger.PaymentPubKeyHash
        , gpDeadline :: LedgerApiV2.POSIXTime
        , getRedeem :: Integer
    } deriving (GHCGenerics.Generic, DataAeson.ToJSON, DataAeson.FromJSON, DataOpenApiSchema.ToSchema)


type BenefTypeSchema = 
    PlutusContract.Endpoint "produce" ProduceParams 
    PlutusContract..\/ PlutusContract.Endpoint "consume" ConsumeParams

produce :: forall w s e. PlutusContract.AsContractError e => ProduceParams -> PlutusContract.Contract w s e ()
produce pp = do
    let p = OnChain.BeneParam
            {
              OnChain.creator = ppCreator pp
            , OnChain.beneficiary = ppBeneficiary pp
            , OnChain.deadline = ppDeadline pp
            }
        d = OnChain.Dat { OnChain.guess = ppGuess pp }
        value = ppAmount pp
    PlutusContract.logInfo @P.String "------------------------produce start initialize------------------------"
    let tx = Constraints.mustPayToOtherScriptWithDatumInTx (OnChain.simpleHash p) (ScriptsLedger.Datum $ PlutusTx.toBuiltinData d) (lovelaceValueOf value)
        lookups = Constraints.plutusV2OtherScript $ OnChain.validator p

    submittedTx <- PlutusContract.submitTxConstraintsWith @OnChain.Benef lookups tx
    Monad.void $ PlutusContract.awaitTxConfirmed $ LedgerTx.getCardanoTxId submittedTx
    PlutusContract.logInfo @P.String "------------------------Transaction processed------------------------"
        

consume :: forall w s. ConsumeParams -> PlutusContract.Contract w s DataText.Text ()
consume ConsumeParams{..} = do 

    pkh <- PlutusContract.ownFirstPaymentPubKeyHash
    (tconstraint ,_) <- PlutusContract.currentNodeClientTimeRange
    if tconstraint < gpDeadline
        then PlutusContract.logInfo @P.String $ TextPrintf.printf "Deadline not yet reached- current time is %s" (P.show tconstraint )
    else do
        let p = OnChain.BeneParam
                {
                OnChain.creator = gpCreator
                , OnChain.beneficiary = pkh
                , OnChain.deadline = gpDeadline
                }
        maybeutxo <- findUtxoInValidator p getRedeem
        case maybeutxo of 
            Nothing -> PlutusContract.logInfo @P.String $ TextPrintf.printf "Not possible to retrieve"
            Just (oref, o) -> do
                PlutusContract.logInfo @P.String $ TextPrintf.printf "Redeem utxos %s - with timing tconstraint at %s:" (P.show oref) (P.show $ tconstraint)
                let r = OnChain.Redeem{ OnChain.redeem = getRedeem }
                    lookups = Constraints.unspentOutputs (Map.singleton oref o) P.<>
                            Constraints.plutusV2OtherScript (OnChain.validator p)
                    tx = Constraints.mustSpendScriptOutput oref (ScriptsLedger.Redeemer $ PlutusTx.toBuiltinData r) P.<>
                        Constraints.mustValidateIn (LedgerApiV2.from tconstraint) P.<>
                        Constraints.mustPayToPubKey gpCreator (Ada.toValue (getTotalValuePay o)) P.<>
                        Constraints.mustBeSignedBy pkh
                submittedTx <- PlutusContract.submitTxConstraintsWith @OnChain.Benef lookups tx
                Monad.void $ PlutusContract.awaitTxConfirmed $ LedgerTx.getCardanoTxId submittedTx

--Utils file functions
getDatum :: (LedgerApiV2.TxOutRef, LedgerTx.DecoratedTxOut) -> Maybe OnChain.Dat
getDatum (_, o) = do
    (_, datumFromQuery) <- o ControlLens.^? LedgerTx.decoratedTxOutDatum
    LedgerApiV2.Datum e <- datumFromQuery ControlLens.^? LedgerTx.datumInDatumFromQuery

    case (LedgerApiV2.fromBuiltinData e :: Maybe OnChain.Dat) of    
        Nothing -> Nothing
        d -> d

checkUTXO :: (LedgerApiV2.TxOutRef, LedgerTx.DecoratedTxOut) -> Integer -> Bool
checkUTXO (oref,o) n = do
    case getDatum (oref,o) of
        Nothing -> False
        Just OnChain.Dat{..}
            | guess == n -> True
            | otherwise  -> False

findUTXO :: [(LedgerApiV2.TxOutRef, LedgerTx.DecoratedTxOut)] ->  Integer -> (Maybe (LedgerApiV2.TxOutRef, LedgerTx.DecoratedTxOut))
findUTXO [] _ = Nothing
findUTXO [(oref,o)] n  = do
    if checkUTXO (oref, o) n then 
        return (oref,o)
    else 
        Nothing
findUTXO ((oref,o):xs) n
    | checkUTXO (oref , o)  n= return (oref,o)
    | otherwise = findUTXO xs n

findUtxoInValidator :: OnChain.BeneParam -> Integer -> PlutusContract.Contract w s DataText.Text (Maybe (LedgerApiV2.TxOutRef, LedgerTx.DecoratedTxOut))
findUtxoInValidator p n = do
    utxos <- PlutusContract.utxosAt $ OnChain.simpleAddress p
    let 
        xs = [ (oref, o) | (oref, o) <- Map.toList utxos ]
        out = findUTXO xs n
    return out

getTotalValuePay :: LedgerTx.DecoratedTxOut -> Ada.Ada 
getTotalValuePay o = do 
    (Ada.fromValue $ LedgerTx._decoratedTxOutValue o) `Ada.divide` 10 

endpoints :: PlutusContract.Contract () BenefTypeSchema DataText.Text ()
endpoints = PlutusContract.awaitPromise  (produce' `PlutusContract.select` consume') >> endpoints
    where 
        produce' = PlutusContract.endpoint @"produce" produce
        consume' = PlutusContract.endpoint @"consume" consume
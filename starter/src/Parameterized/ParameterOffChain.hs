
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
{-# LANGUAGE RecordWildCards #-} -- To allow notation like Getparams {..}

module Parameterized.ParameterOffChain where

-- Haskell imports
import qualified Control.Monad            as Monad (void)
import qualified GHC.Generics                        as GHCGenerics (Generic)
import qualified Data.Aeson                          as DataAeson (ToJSON, FromJSON)
import qualified Data.OpenApi.Schema                 as DataOpenApiSchema (ToSchema)
import qualified Prelude                  as P
import qualified Data.Map                 as Map
import qualified Data.Text                           as DataText (Text)

-- Plutus imports
import qualified PlutusTx
import PlutusTx.Prelude
import qualified Plutus.Contract          as PlutusContract
import qualified Ledger.Ada               as Ada
import qualified Ledger.Tx                as LedgerTx
import qualified Plutus.V2.Ledger.Api                            as LedgerApiV2
import qualified Ledger                                          (PaymentPubKeyHash, Value)
import qualified Text.Printf              as TextPrintf (printf)
import qualified Ledger.Constraints       as Constraints
import qualified Plutus.V1.Ledger.Scripts as ScriptsLedger

--Own modules
import qualified Parameterized.ParameterOnChain as OnChain

data StartParams = StartParams
    { spCreator     :: !Ledger.PaymentPubKeyHash
    , spBeneficiary :: !Ledger.PaymentPubKeyHash
    , spDeadline    :: !LedgerApiV2.POSIXTime
    , spGuess       :: !Integer
    , spAmount      :: !Integer
    } deriving (GHCGenerics.Generic, DataAeson.ToJSON, DataAeson.FromJSON, DataOpenApiSchema.ToSchema)

data GrabParams = GrabParams 
    {
          gpCreator  :: !Ledger.PaymentPubKeyHash
        , gpDeadline :: !LedgerApiV2.POSIXTime
        , gpGuess    :: !Integer
    } deriving (GHCGenerics.Generic, DataAeson.ToJSON, DataAeson.FromJSON, DataOpenApiSchema.ToSchema)


type BeneSchema =
    PlutusContract.Endpoint "Start" StartParams
    PlutusContract..\/ PlutusContract.Endpoint "Grab" GrabParams

start :: PlutusContract.AsContractError e => StartParams -> PlutusContract.Contract w s e ()
start sp = do
    let p = OnChain.BenParam
            {
                OnChain.creator = spCreator sp,
                OnChain.beneficiary = spBeneficiary sp,
                OnChain.deadline = spDeadline sp
            }
        d = OnChain.Dat { OnChain.ddata = spGuess sp}
        v = Ada.lovelaceValueOf $ spAmount sp
        txConstraints = Constraints.mustPayToOtherScript (OnChain.validatorHash p) (LedgerApiV2.Datum $ PlutusTx.toBuiltinData d) v
        lookups = Constraints.plutusV2OtherScript $ OnChain.validator p
        scriptAddress = OnChain.address p
        scriptHash = OnChain.validatorHash p


-- the final goal is to build and submit the transaction
    submittedTx <- PlutusContract.submitTxConstraintsWith @OnChain.Simple lookups txConstraints
    Monad.void $ PlutusContract.awaitTxConfirmed $ LedgerTx.getCardanoTxId submittedTx
    PlutusContract.logInfo @P.String $ TextPrintf.printf "--------------------------- Start Endpoint - Submited - Datum: %s - Value: %s ---------------------------" (P.show d) (P.show v)
    PlutusContract.logInfo @P.String $ TextPrintf.printf "--------------------------- Script with Address: %s and Hash: %s ---------------------------" (P.show scriptAddress) (P.show scriptHash)

grab :: forall w s. GrabParams -> PlutusContract.Contract w s DataText.Text ()
grab GrabParams{..} = do
    beneficiary <- PlutusContract.ownFirstPaymentPubKeyHash
    now <- PlutusContract.currentTime
    if now < gpDeadline
        then PlutusContract.logInfo @P.String $ TextPrintf.printf "Deadline not yet reached - Deadline: %s - Now: %s" (P.show gpDeadline) (P.show now)
        else do
            let param = OnChain.BenParam {
                        OnChain.creator = gpCreator
                      , OnChain.beneficiary = beneficiary
                      , OnChain.deadline = gpDeadline
                }
                r = OnChain.Redeem { OnChain.redeem = gpGuess }
            maybeutxo <- findUtxoInValidator param gpGuess --finds the utxos associated to the beneficiary that have valid deadline and guess number
            case maybeutxo of
                Nothing -> PlutusContract.logInfo @P.String $ TextPrintf.printf "Wrong guess %s or not deadline reached %s and pubkey %s" (P.show r) (P.show $ now) (P.show $ OnChain.beneficiary param)
                Just (oref, o) -> do
                    PlutusContract.logInfo @P.String $ TextPrintf.printf "Redeem utxos %s - with timing now at %s:" (P.show oref) (P.show $ now)
                    let lookups = Constraints.unspentOutputs (Map.singleton oref o) P.<>
                                  Constraints.plutusV2OtherScript (OnChain.validator param)
                        tx = Constraints.mustSpendScriptOutput oref (ScriptsLedger.Redeemer $ PlutusTx.toBuiltinData r) P.<>
                            Constraints.mustValidateIn (LedgerApiV2.from now) P.<>
                            Constraints.mustPayToPubKey gpCreator (getTotalValuePay o)
                    submittedTx <- PlutusContract.submitTxConstraintsWith @OnChain.Simple lookups tx
                    Monad.void $ PlutusContract.awaitTxConfirmed $ LedgerTx.getCardanoTxId submittedTx
                    PlutusContract.logInfo @P.String $ "collected gifts"

getDatum :: (LedgerApiV2.TxOutRef, LedgerTx.ChainIndexTxOut) -> Maybe OnChain.Dat
getDatum (_, o) = do
    let 
        datHashOrDatum = LedgerTx._ciTxOutScriptDatum o

    LedgerApiV2.Datum e <- snd datHashOrDatum
    
    case (LedgerApiV2.fromBuiltinData e :: Maybe OnChain.Dat) of    
        Nothing -> Nothing
        d -> d

checkUTXO :: (LedgerApiV2.TxOutRef, LedgerTx.ChainIndexTxOut) -> Integer -> Bool
checkUTXO (oref,o) n = do
    case getDatum (oref,o) of
        Nothing -> False
        Just OnChain.Dat{..}
            | ddata == n -> True
            | otherwise  -> False

findUTXO :: [(LedgerApiV2.TxOutRef, LedgerTx.ChainIndexTxOut)] -> Integer -> (Maybe (LedgerApiV2.TxOutRef, LedgerTx.ChainIndexTxOut))
findUTXO [] _ = Nothing
findUTXO [(oref,o)] n = do
    if checkUTXO (oref, o) n then 
        return (oref, o)
    else 
        Nothing
findUTXO ((oref,o):xs) n
    | checkUTXO (oref ,o)  n = return (oref, o)
    | otherwise = findUTXO xs n

findUtxoInValidator :: OnChain.BenParam -> Integer -> PlutusContract.Contract w s DataText.Text (Maybe (LedgerApiV2.TxOutRef, LedgerTx.ChainIndexTxOut))
findUtxoInValidator gparam n = do
    utxos <- PlutusContract.utxosAt $ OnChain.address gparam
    let 
        xs = [ (oref, o) | (oref, o) <- Map.toList utxos ]
        out = findUTXO xs n
    return out

getTotalValuePay :: LedgerTx.ChainIndexTxOut -> Ledger.Value
getTotalValuePay o = do
    Ada.toValue $ (Ada.fromValue $ LedgerTx._ciTxOutValue o) `Ada.divide` 10
    -- return tValue

-- This puts all together. The select means to offer selection to the user. 
endpoints :: PlutusContract.Contract () BeneSchema DataText.Text ()
endpoints = PlutusContract.awaitPromise (start' `PlutusContract.select` grab') >> endpoints
    where 
        start' = PlutusContract.endpoint @"Start" start
        grab' = PlutusContract.endpoint @"Grab" $ grab
-- 1. dependecies
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TemplateHaskell #-} 
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE ScopedTypeVariables #-}

{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE TypeOperators       #-}
{-# LANGUAGE OverloadedStrings   #-}

-- 2. imports external/imports

module Beneficiary.OnChain where

import qualified Prelude                                as P
import qualified Plutus.Script.Utils.V2.Typed.Scripts.Validators as Scripts
import qualified Plutus.Script.Utils.V2.Scripts as V2UtilsScripts
import qualified Plutus.V2.Ledger.Contexts                       as Contexts
import qualified Plutus.V2.Ledger.Api                            as LedgerApiV2
import qualified Plutus.V1.Ledger.Interval                            as LedgerIntervalV1
import qualified Ledger                                          
import qualified PlutusTx
import PlutusTx.Prelude

data Beneficiary = Beneficiary
    {
        beneficiary :: Ledger.PaymentPubKeyHash
        , deadline :: LedgerApiV2.POSIXTime
        , guess :: Integer
    } 

PlutusTx.unstableMakeIsData ''Beneficiary

newtype Redeem = Redeem 
    {
        redeem :: Integer
    } deriving P.Show

PlutusTx.unstableMakeIsData ''Redeem

data Benef 
instance Scripts.ValidatorTypes Benef where
    type instance RedeemerType Benef = Redeem
    type instance DatumType Benef = Beneficiary

-- 3. onchain code
{-# INLINABLE benefValidator #-}
benefValidator :: Beneficiary -> Redeem -> Contexts.ScriptContext -> Bool
benefValidator d r context = 
    traceIfFalse "Failure to guess" (guess d == redeem r) &&
    traceIfFalse "Not signed by beneficiary" signedByBeneficiary &&
    traceIfFalse "Deadline not yet reached" deadlinepassed
    
    where 
        txinfo :: Contexts.TxInfo 
        txinfo = Contexts.scriptContextTxInfo context

        signedByBeneficiary :: Bool
        signedByBeneficiary = Contexts.txSignedBy txinfo $ Ledger.unPaymentPubKeyHash (beneficiary d)

        deadlinepassed :: Bool 
        deadlinepassed = LedgerIntervalV1.contains (LedgerIntervalV1.from (deadline d)) (Contexts.txInfoValidRange txinfo)


benefCompile :: Scripts.TypedValidator Benef 
benefCompile = Scripts.mkTypedValidator @Benef 
    $$(PlutusTx.compile [|| benefValidator ||])
    $$(PlutusTx.compile [|| wrap ||]) where 
        wrap = Scripts.mkUntypedValidator @Beneficiary @Redeem
--------------------------------OnChain code


--------------------------------
validator :: V2UtilsScripts.Validator
validator = Scripts.validatorScript benefCompile

simpleHash :: V2UtilsScripts.ValidatorHash
simpleHash = V2UtilsScripts.validatorHash validator

simpleAddress :: Ledger.Address
simpleAddress = Ledger.scriptHashAddress simpleHash

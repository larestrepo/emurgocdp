-- 1. dependecies
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TemplateHaskell #-} 
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE ScopedTypeVariables #-}

{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE TypeOperators       #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE MultiParamTypeClasses      #-} 

-- 2. imports external/imports

module Parameterized.OnChain where

import qualified Prelude                                as P
import qualified Plutus.Script.Utils.V2.Typed.Scripts.Validators as Scripts
import qualified Plutus.Script.Utils.V2.Scripts as V2UtilsScripts
import qualified Plutus.V2.Ledger.Contexts                       as Contexts
import qualified Plutus.V2.Ledger.Api                            as LedgerApiV2
import qualified Plutus.V1.Ledger.Interval                            as LedgerIntervalV1
import qualified Ledger                                          
import qualified PlutusTx
import PlutusTx.Prelude
import qualified Ledger.Ada                                      as Ada

data BeneParam = BeneParam
    {
        creator :: Ledger.PaymentPubKeyHash,
        beneficiary :: Ledger.PaymentPubKeyHash,
        deadline :: LedgerApiV2.POSIXTime
    } deriving P.Show

PlutusTx.unstableMakeIsData ''BeneParam
PlutusTx.makeLift ''BeneParam

newtype Dat = Dat 
    {
        guess :: Integer
    } deriving P.Show
PlutusTx.unstableMakeIsData ''Dat

newtype Redeem = Redeem 
    {
        redeem :: Integer
    } deriving P.Show

PlutusTx.unstableMakeIsData ''Redeem

data Benef 
instance Scripts.ValidatorTypes Benef where
    type instance RedeemerType Benef = Redeem
    type instance DatumType Benef = Dat

-- 3. onchain code
{-# INLINABLE benefValidator #-}
benefValidator :: BeneParam -> Dat -> Redeem -> Contexts.ScriptContext -> Bool
benefValidator benefp d r context = 
    traceIfFalse "Failure to guess" (guess d == redeem r) &&
    traceIfFalse "Not signed by beneficiary" signedByBeneficiary &&
    traceIfFalse "Deadline not yet reached" deadlinepassed &&
    traceIfFalse "Royalties not provided" calculateRoyalties
    
    where 
        txinfo :: Contexts.TxInfo 
        txinfo = Contexts.scriptContextTxInfo context

        signedByBeneficiary :: Bool
        signedByBeneficiary = Contexts.txSignedBy txinfo $ Ledger.unPaymentPubKeyHash (beneficiary benefp)

        deadlinepassed :: Bool 
        deadlinepassed = LedgerIntervalV1.contains (LedgerIntervalV1.from (deadline benefp)) (Contexts.txInfoValidRange txinfo)

        adaroyalties :: Maybe Ada.Ada
        adaroyalties = do 
            validatedValue <- Contexts.txOutValue . Contexts.txInInfoResolved <$> Contexts.findOwnInput context
            Just $ Ada.fromValue validatedValue `Ada.divide` 10

        getValuePaidToCreator :: Ada.Ada 
        getValuePaidToCreator = Ada.fromValue $ Contexts.valuePaidTo txinfo (Ledger.unPaymentPubKeyHash (creator benefp))

        compareValues :: Ada.Ada -> Maybe Ada.Ada -> Bool
        compareValues _ Nothing = False
        compareValues vToCreator adaroy = Just vToCreator >= adaroy

        calculateRoyalties :: Bool
        calculateRoyalties = compareValues getValuePaidToCreator adaroyalties

benefCompile :: BeneParam -> Scripts.TypedValidator Benef 
benefCompile benefp = Scripts.mkTypedValidator @Benef 
    ($$(PlutusTx.compile [|| benefValidator ||]) `PlutusTx.applyCode` PlutusTx.liftCode benefp)
    $$(PlutusTx.compile [|| wrap ||]) where 
        wrap = Scripts.mkUntypedValidator @Dat @Redeem
--------------------------------OnChain code


--------------------------------
validator :: BeneParam -> V2UtilsScripts.Validator
validator = Scripts.validatorScript . benefCompile

simpleHash :: BeneParam -> V2UtilsScripts.ValidatorHash
simpleHash = V2UtilsScripts.validatorHash . validator

simpleAddress :: BeneParam -> Ledger.Address
simpleAddress = Ledger.scriptHashAddress . simpleHash


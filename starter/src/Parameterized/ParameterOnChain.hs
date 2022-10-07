--1 Extensions
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DataKinds #-}

--This is to work not only with Strings
{-# LANGUAGE OverloadedStrings   #-}

-- required to use custom data types
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE TypeOperators       #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE MultiParamTypeClasses      #-} 


module Parameterized.ParameterOnChain where
-- Sections of a Plutus contract


--2 Imports
import PlutusTx
import PlutusTx.Prelude
import qualified Ledger.Address                                  as V1LAddress
import qualified Plutus.V2.Ledger.Api                            as V2LedgerApi
import qualified Plutus.V2.Ledger.Contexts                       as Contexts
import qualified Plutus.Script.Utils.V2.Typed.Scripts.Validators as V2UtilsTypeScripts
import qualified Prelude                                         as P
import qualified Ledger                                          (PaymentPubKeyHash, unPaymentPubKeyHash)
import qualified Plutus.V1.Ledger.Interval                            as LedgerIntervalV1
import qualified Ledger.Ada                                      as Ada

--3 Onchain code

data BenParam = BenParam
    {
    creator :: Ledger.PaymentPubKeyHash,
    beneficiary :: Ledger.PaymentPubKeyHash,
    deadline :: V2LedgerApi.POSIXTime
    } deriving P.Show

PlutusTx.unstableMakeIsData ''BenParam
PlutusTx.makeLift ''BenParam

newtype Redeem = Redeem
    {
        redeem :: Integer
    } deriving P.Show

PlutusTx.unstableMakeIsData ''Redeem -- This is to instantiate the IsData class

data Dat = Dat 
    {
        ddata :: Integer
    } deriving P.Show

PlutusTx.unstableMakeIsData ''Dat

{-# INLINABLE simpleType #-}
--Actual validator logic
simpleType :: BenParam -> Dat -> Redeem -> Contexts.ScriptContext -> Bool
simpleType benp d r context = 
    traceIfFalse "Sorry the guess is not correct" (ddata d == redeem r) &&
    traceIfFalse "Wrong pubkeyhash" signedByBeneficiary &&
    traceIfFalse "Deadline not yet reached"  deadlinepassed &&
    traceIfFalse "Not paid royalties"  calculateRoyalties
    where
        txinfo :: Contexts.TxInfo
        txinfo = Contexts.scriptContextTxInfo context

        signedByBeneficiary :: Bool
        signedByBeneficiary = Contexts.txSignedBy txinfo $ Ledger.unPaymentPubKeyHash (beneficiary benp)

        deadlinepassed :: Bool
        deadlinepassed = LedgerIntervalV1.contains (LedgerIntervalV1.from (deadline benp)) (Contexts.txInfoValidRange txinfo)
        
        adaroyalties :: Maybe Ada.Ada
        adaroyalties = do
            validatedValue <- Contexts.txOutValue . Contexts.txInInfoResolved <$> Contexts.findOwnInput context
            Just $ Ada.fromValue validatedValue `Ada.divide` 10

        getValuePaidToCreator :: Ada.Ada
        getValuePaidToCreator = Ada.fromValue $ Contexts.valuePaidTo txinfo (Ledger.unPaymentPubKeyHash (creator benp))

        compareValues :: Ada.Ada -> Maybe Ada.Ada -> Bool
        -- compareValues Nothing _ = False
        compareValues vToCreator adaTx = Just (vToCreator) >= adaTx

        calculateRoyalties :: Bool
        calculateRoyalties = compareValues (getValuePaidToCreator) (adaroyalties)

data Simple
instance V2UtilsTypeScripts.ValidatorTypes Simple where
    type instance RedeemerType Simple = Redeem
    type instance DatumType Simple = Dat

--Boilerplate
simpleTypeV :: BenParam -> V2UtilsTypeScripts.TypedValidator Simple
simpleTypeV benp = V2UtilsTypeScripts.mkTypedValidator @Simple 
    ($$(compile [|| simpleType ||]) `PlutusTx.applyCode` PlutusTx.liftCode benp)
    $$(compile [|| wrap ||]) where
        wrap = V2UtilsTypeScripts.mkUntypedValidator @Dat @Redeem

validator :: BenParam -> V2LedgerApi.Validator
validator = V2UtilsTypeScripts.validatorScript . simpleTypeV

validatorHash :: BenParam -> V2LedgerApi.ValidatorHash
validatorHash = V2UtilsTypeScripts.validatorHash . simpleTypeV

address :: BenParam -> V1LAddress.Address
address = V1LAddress.scriptHashAddress . validatorHash

-- {-# INLINABLE validateRoyalties #-}
-- validateRoyalties :: BenParam -> Contexts.TxInfo -> Bool
-- validateRoyalties benp txinfo = compareValues (qCreator benp txinfo) (totalValue txinfo)

-- -- Get total amount ADA from the transaction
-- {-# INLINABLE totalValue #-}
-- totalValue :: Contexts.TxInfo -> Ada.Ada
-- totalValue txinfo = Ada.fromValue $ Contexts.valueSpent txinfo

-- --Get Value paid to the creator of the contract (10%)
-- {-# INLINABLE qCreator #-}
-- qCreator :: BenParam -> Contexts.TxInfo -> Ada.Ada
-- qCreator benp txinfo = Ada.fromValue $ Contexts.valuePaidTo txinfo (Ledger.unPaymentPubKeyHash (creator benp))

-- {-# INLINABLE compareValues #-}
-- compareValues :: Ada.Ada -> Ada.Ada -> Bool
-- -- compareValues Nothing _ = False
-- compareValues valueToCreator valueTotal = valueToCreator >= valueTotal `Ada.divide` 10
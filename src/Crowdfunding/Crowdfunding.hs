
{-# LANGUAGE DataKinds #-} --Makes the kind system extensible
{-# LANGUAGE TemplateHaskell #-} --allows you to do type-safe compile-time meta-programming
{-# LANGUAGE NoImplicitPrelude   #-} --PlutusTx prelude has priority over Haskell prelude
{-# LANGUAGE ScopedTypeVariables #-}

{-# LANGUAGE TypeApplications    #-} --required to use custo datatypes
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE TypeOperators       #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE MultiParamTypeClasses      #-} -- required to avoid error of using MultiParamTypeClasses that lift uses

module Crowdfunding.Crowdfunding where

-- Module imports
import qualified Prelude                                         as P
import qualified Plutus.Script.Utils.V2.Typed.Scripts.Validators as V2UtisTypeScripts
import qualified Plutus.V2.Ledger.Contexts                       as Contexts
import qualified Plutus.V2.Ledger.Api                            as LedgerApiV2
import qualified Plutus.V1.Ledger.Address                            as LedgerAddressV1
import qualified Plutus.V1.Ledger.Credential                            as LedgerCredentialV1
import qualified Plutus.V1.Ledger.Interval                       as LedgerIntervalV1
import qualified Ledger                                          (PaymentPubKeyHash, unPaymentPubKeyHash)
import qualified PlutusTx
import PlutusTx.Prelude
import qualified Ledger.Ada                                      as Ada


-- Define the data type
data CrowdParam = CrowdParam 
    {
        beneficiary :: Ledger.PaymentPubKeyHash,
        targetAmount :: Ada.Ada,
        deadline    :: LedgerApiV2.POSIXTime
    } 
    deriving P.Show

--Instantiate the data type to IsData
PlutusTx.unstableMakeIsData ''CrowdParam
-- Instantiate the Lift Class to the data type
PlutusTx.makeLift ''CrowdParam

newtype Dat = Dat {contributor :: Ledger.PaymentPubKeyHash} 
    deriving P.Show

PlutusTx.unstableMakeIsData ''Dat

-- Options are:
-- Collect: if the deadline has not been reached a contributor can get back the Ada
-- Close: when the campaign has ended succesfully, it is possible to redeem the funds and send it to the beneficiary
data CrowdRedeemer = Collect | Close
    deriving P.Show

PlutusTx.makeIsDataIndexed ''CrowdRedeemer 
    [ ('Collect, 0),
      ('Close, 1)
    ]
PlutusTx.makeLift ''CrowdRedeemer

{-# INLINABLE validation #-}

--Actual Plutus validation logic
validation :: CrowdParam -> Dat -> CrowdRedeemer -> Contexts.ScriptContext -> Bool
validation crp d r context = 
    case r of 
        Collect    -> traceIfFalse "Deadline passed to collect" (not deadlinepassed) 
                   && traceIfFalse "Not signed by contributor" signedByContributor
                   && traceIfFalse "Target amount reached" (not targetAmountReached)
        Close      -> traceIfFalse "Not signed by beneficiary" signedByBeneficiary 
                   && traceIfFalse "Deadline not yet reached" deadlinepassed 
                   && traceIfFalse "Target amount not yet reached" targetAmountReached

    where
        txinfo :: Contexts.TxInfo
        txinfo = Contexts.scriptContextTxInfo context

        signedByBeneficiary :: Bool
        signedByBeneficiary = Contexts.txSignedBy txinfo $ Ledger.unPaymentPubKeyHash (beneficiary crp)

        signedByContributor :: Bool
        signedByContributor = Contexts.txSignedBy txinfo $ Ledger.unPaymentPubKeyHash (contributor d)

        deadlinepassed :: Bool
        deadlinepassed = LedgerIntervalV1.contains (LedgerIntervalV1.from (deadline crp)) (Contexts.txInfoValidRange txinfo) 

        txInfoInputs :: [Contexts.TxInInfo]
        txInfoInputs = Contexts.txInfoInputs txinfo
        ownInputsAdaSum :: [Contexts.TxInInfo] -> Ada.Ada
        ownInputsAdaSum [] = mempty
        ownInputsAdaSum (input:inputs) =
            let utxo = Contexts.txInInfoResolved input
                adaInUtxo = Ada.fromValue $ Contexts.txOutValue utxo -- It gets all the utxo injected into the transaction
                utxoAddress = LedgerAddressV1.addressCredential $ Contexts.txOutAddress utxo -- We need to get the associated address of those utxos. type: Credential.
                campaignAddress = LedgerCredentialV1.ScriptCredential $ Contexts.ownHash context -- Get the address of the script and then make it of type Credential.
            in (if utxoAddress == campaignAddress then adaInUtxo else mempty) + ownInputsAdaSum inputs
        targetAmountReached :: Bool
        targetAmountReached = ownInputsAdaSum txInfoInputs >= targetAmount crp

--Interface to help the PlutusTx to know data types of datum and redeemer.

data Crowdfunding
instance V2UtisTypeScripts.ValidatorTypes Crowdfunding where
    type instance DatumType Crowdfunding = Dat
    type instance RedeemerType Crowdfunding = CrowdRedeemer


typeValidator :: CrowdParam -> V2UtisTypeScripts.TypedValidator Crowdfunding
typeValidator crp = V2UtisTypeScripts.mkTypedValidator @Crowdfunding
    ($$(PlutusTx.compile [|| validation ||]) `PlutusTx.applyCode` PlutusTx.liftCode crp)
    $$(PlutusTx.compile [|| wrap ||])
  where
    wrap = V2UtisTypeScripts.mkUntypedValidator @Dat @CrowdRedeemer

----------------
validator :: CrowdParam -> LedgerApiV2.Validator
validator = V2UtisTypeScripts.validatorScript . typeValidator

typeValidatorHash :: CrowdParam -> LedgerApiV2.ValidatorHash
typeValidatorHash = V2UtisTypeScripts.validatorHash . typeValidator

-- | The address of the game (the hash of its validator script)
myTypeAddress :: CrowdParam -> LedgerApiV2.Address
myTypeAddress = V2UtisTypeScripts.validatorAddress . typeValidator
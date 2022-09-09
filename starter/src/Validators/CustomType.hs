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


module Validators.CustomType where
-- Sections of a Plutus contract


--2 Imports
import PlutusTx
import PlutusTx.Prelude
import qualified Ledger.Address as V1LAddress
import qualified Plutus.V2.Ledger.Api as V2LedgerApi
import qualified Plutus.V2.Ledger.Contexts                       as Contexts
import qualified Plutus.Script.Utils.V2.Typed.Scripts.Validators as V2UtilsTypeScripts

--3 Onchain code

newtype Redeem = Redeem Integer
PlutusTx.unstableMakeIsData ''Redeem

data Simple
instance V2UtilsTypeScripts.ValidatorTypes Simple where
    type instance RedeemerType Simple = Redeem
    type instance DatumType Simple = ()



{-# INLINABLE simpleType #-}
--Actual validator logic
simpleType :: () -> Redeem -> Contexts.ScriptContext -> Bool
simpleType _ (Redeem i) _ = traceIfFalse "Sorry the guess is not correct" (i == 300)


--Boilerplate
simpleTypeV :: V2UtilsTypeScripts.TypedValidator Simple
simpleTypeV = V2UtilsTypeScripts.mkTypedValidator @Simple 
    $$(compile [|| simpleType ||])
    $$(compile [|| wrap ||]) where
        wrap = V2UtilsTypeScripts.mkUntypedValidator @() @Redeem

validator :: V2LedgerApi.Validator
validator = V2UtilsTypeScripts.validatorScript simpleTypeV

validatorHash :: V2LedgerApi.ValidatorHash
validatorHash = V2UtilsTypeScripts.validatorHash simpleTypeV

address :: V1LAddress.Address
address = V1LAddress.scriptHashAddress validatorHash











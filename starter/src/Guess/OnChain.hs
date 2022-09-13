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


module Guess.OnChain where
-- Sections of a Plutus contract


--2 Imports
import PlutusTx
import PlutusTx.Prelude
import qualified Ledger.Address                                  as V1LAddress
import qualified Plutus.V2.Ledger.Api                            as V2LedgerApi
import qualified Plutus.V2.Ledger.Contexts                       as Contexts
import qualified Plutus.Script.Utils.V2.Typed.Scripts.Validators as V2UtilsTypeScripts
import qualified Prelude                                         as P

--3 Onchain code

newtype Redeem = Redeem
    {
        redeem :: Integer
    } deriving P.Show

PlutusTx.unstableMakeIsData ''Redeem -- This is to instantiate the IsData class

newtype Dat = Dat 
    {
        ddata :: Integer
    } deriving P.Show

PlutusTx.unstableMakeIsData ''Dat

data Simple
instance V2UtilsTypeScripts.ValidatorTypes Simple where
    type instance RedeemerType Simple = Redeem
    type instance DatumType Simple = Dat

{-# INLINABLE simpleType #-}
--Actual validator logic
simpleType :: Dat -> Redeem -> Contexts.ScriptContext -> Bool
simpleType d r _ = traceIfFalse "Sorry the guess is not correct" (ddata d == redeem r)


--Boilerplate
simpleTypeV :: V2UtilsTypeScripts.TypedValidator Simple
simpleTypeV = V2UtilsTypeScripts.mkTypedValidator @Simple 
    $$(compile [|| simpleType ||])
    $$(compile [|| wrap ||]) where
        wrap = V2UtilsTypeScripts.mkUntypedValidator @Dat @Redeem

validator :: V2LedgerApi.Validator
validator = V2UtilsTypeScripts.validatorScript simpleTypeV

validatorHash :: V2LedgerApi.ValidatorHash
validatorHash = V2UtilsTypeScripts.validatorHash simpleTypeV

address :: V1LAddress.Address
address = V1LAddress.scriptHashAddress validatorHash
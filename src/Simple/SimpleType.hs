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

module Simple.SimpleType where

import PlutusTx.Prelude
import qualified Ledger
-- import qualified Plutus.V2.Ledger.Api as Scripts
import qualified Plutus.Script.Utils.V2.Typed.Scripts as Scripts
import qualified Plutus.Script.Utils.V2.Scripts as V2UtilsScripts
import qualified Plutus.V2.Ledger.Contexts as Contexts
import PlutusTx

data Simple 
instance Scripts.ValidatorTypes Simple where
    type instance RedeemerType Simple = Integer
    type instance DatumType Simple = ()

-- 3. onchain code
{-# INLINABLE simple #-}
simple :: () -> Integer -> Contexts.ScriptContext -> Bool
simple _ i _ = traceIfFalse "Failure to guess" (i==100)


simpleCompile :: Scripts.TypedValidator Simple 
simpleCompile = Scripts.mkTypedValidator @Simple 
    $$(compile [|| simple ||])
    $$(compile [|| wrap ||]) where 
        wrap = Scripts.mkUntypedValidator @() @Integer
--------------------------------OnChain code


--------------------------------
validator :: V2UtilsScripts.Validator
validator = Scripts.validatorScript simpleCompile

simpleHash :: V2UtilsScripts.ValidatorHash
simpleHash = V2UtilsScripts.validatorHash validator

simpleAddress :: Ledger.Address
simpleAddress = Ledger.scriptHashAddress simpleHash

{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DataKinds #-}

module Validators.SimpleV where
-- Sections of a Plutus contract

--1 Extensions

--2 Imports
import PlutusTx
import PlutusTx.Prelude
import qualified Ledger
import qualified Plutus.V2.Ledger.Api as V2LedgerApi
import qualified Plutus.Script.Utils.V2.Scripts as V2UtilsScripts

--3 Onchain code

--Actual validator logic
simpleV :: BuiltinData -> BuiltinData -> BuiltinData -> ()
simpleV _ _ _ = error ()

--Boilerplate
simpleVCompiled :: CompiledCode (BuiltinData -> BuiltinData -> BuiltinData -> ())
simpleVCompiled = $$(compile [|| simpleV ||])

simpleValidator :: V2UtilsScripts.Validator
simpleValidator = V2LedgerApi.mkValidatorScript simpleVCompiled

validatorHash :: Ledger.ValidatorHash
validatorHash = Ledger.validatorHash simpleValidator

address :: Ledger.Address
address = Ledger.scriptHashAddress validatorHash










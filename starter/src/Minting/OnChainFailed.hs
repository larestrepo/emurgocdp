{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE TupleSections              #-}
{-# LANGUAGE AllowAmbiguousTypes        #-}
{-# LANGUAGE NumericUnderscores         #-}

module Minting.OnChain where

import qualified Ledger                              as Ledger (CurrencySymbol)
import qualified Plutus.Script.Utils.V1.Scripts      as UtilsScriptsV1 (scriptCurrencySymbol)
import qualified Plutus.Script.Utils.V1.Typed.Scripts.MonetaryPolicies as UtilsTypedScriptsMintingV1
import qualified Plutus.V1.Ledger.Scripts            as LedgerScriptsV1
import qualified Plutus.V1.Ledger.Contexts           as LedgerContextsV1  (ScriptContext)
import qualified PlutusTx
import           PlutusTx.Prelude                    hiding (unless)
import           Plutus.V1.Ledger.Api                as LedgerApiV1

{-# INLINABLE mkPolicy #-}
mkPolicy :: () -> LedgerContextsV1.ScriptContext -> Bool
mkPolicy () _ = True

policy :: LedgerScriptsV1.MintingPolicy
policy = LedgerScriptsV1.mkMintingPolicyScript $$(PlutusTx.compile [|| UtilsTypedScriptsMintingV1.mkUntypedMintingPolicy mkPolicy ||])

curSymbol :: Ledger.CurrencySymbol
curSymbol = UtilsScriptsV1.scriptCurrencySymbol policy

policyScript :: LedgerApiV1.Script
policyScript = LedgerApiV1.unMintingPolicyScript policy

mintValidator :: LedgerScriptsV1.Validator
mintValidator = LedgerScriptsV1.Validator policyScript

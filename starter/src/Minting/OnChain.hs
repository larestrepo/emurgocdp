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

--Import Externos

import qualified Ledger                              as Ledger (CurrencySymbol)
import qualified Plutus.Script.Utils.V2.Scripts      as UtilsScriptsV2
import qualified Plutus.Script.Utils.V2.Typed.Scripts.MonetaryPolicies as UtilsTypedScriptsMintingV2
import qualified Plutus.V2.Ledger.Api                as LedgerApiV2
import qualified Plutus.V2.Ledger.Contexts           as LedgerContextsV2  (ScriptContext)
import qualified PlutusTx
import           PlutusTx.Prelude                    hiding (unless)

{-# INLINABLE mkPolicy #-}
mkPolicy :: () -> LedgerContextsV2.ScriptContext -> Bool
mkPolicy () _ = True

policy :: LedgerApiV2.MintingPolicy
policy = LedgerApiV2.mkMintingPolicyScript $$(PlutusTx.compile [|| UtilsTypedScriptsMintingV2.mkUntypedMintingPolicy mkPolicy ||])

curSymbol :: Ledger.CurrencySymbol
curSymbol = UtilsScriptsV2.scriptCurrencySymbol policy

policyScript :: LedgerApiV2.Script
policyScript = LedgerApiV2.unMintingPolicyScript policy

mintValidator :: LedgerApiV2.Validator
mintValidator = LedgerApiV2.Validator policyScript
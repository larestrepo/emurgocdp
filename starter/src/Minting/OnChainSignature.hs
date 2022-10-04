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

import qualified Ledger                              as Ledger
import qualified Plutus.Script.Utils.V2.Typed.Scripts.MonetaryPolicies as UtilsTypedScriptsMintingV2
import qualified Plutus.V2.Ledger.Api                as LedgerApiV2
import qualified Plutus.V2.Ledger.Contexts           as LedgerContextsV2
import qualified PlutusTx
import           PlutusTx.Prelude                    hiding (unless)


newtype SignParam = SignParam
    {
        beneficiary :: Ledger.PaymentPubKeyHash
    }

PlutusTx.unstableMakeIsData ''SignParam
PlutusTx.makeLift ''SignParam

{-# INLINABLE mkPolicy #-}
mkPolicy :: SignParam -> () -> LedgerContextsV2.ScriptContext -> Bool
mkPolicy signparam () context = traceIfFalse "Not signed by the beneficiary" (LedgerContextsV2.txSignedBy (LedgerContextsV2.scriptContextTxInfo context) $ Ledger.unPaymentPubKeyHash $ beneficiary signparam)

policy :: SignParam -> LedgerApiV2.MintingPolicy
policy signparam = LedgerApiV2.mkMintingPolicyScript $
    $$(PlutusTx.compile [|| UtilsTypedScriptsMintingV2.mkUntypedMintingPolicy . mkPolicy ||])
    `PlutusTx.applyCode`
    PlutusTx.liftCode signparam


-- curSymbol :: SignParam -> Ledger.CurrencySymbol
-- curSymbol = UtilsScriptsV2.scriptCurrencySymbol . policy

-- policyScript :: SignParam -> LedgerApiV2.Script
-- policyScript = LedgerApiV2.unMintingPolicyScript . policy

-- mintValidator :: LedgerApiV2.Validator
-- mintValidator = LedgerApiV2.Validator policyScript
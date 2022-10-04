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

module Minting.NFT where

--Import Externos

import qualified Plutus.Script.Utils.V2.Typed.Scripts.MonetaryPolicies as UtilsTypedScriptsMintingV2
import qualified Plutus.V2.Ledger.Api                as LedgerApiV2
import qualified Plutus.V2.Ledger.Contexts           as LedgerContextsV2
import qualified PlutusTx
import           PlutusTx.Prelude                    hiding (unless)
import qualified Plutus.V1.Ledger.Value                                as LedgerValueV1 (flattenValue)


data TokenParam = TokenParam
    {
        utxo :: LedgerApiV2.TxOutRef,
        name :: LedgerApiV2.TokenName
    }

PlutusTx.unstableMakeIsData ''TokenParam
PlutusTx.makeLift ''TokenParam

{-# INLINABLE mkPolicy #-}
mkPolicy :: TokenParam -> () -> LedgerContextsV2.ScriptContext -> Bool
mkPolicy token () context = traceIfFalse "Utxo not consumed" hasUtxo &&
                          traceIfFalse "wrong amount minted" checkMintedAmount
  where
    info :: LedgerContextsV2.TxInfo
    info = LedgerContextsV2.scriptContextTxInfo context 

    hasUtxo :: Bool
    hasUtxo = any (\i -> LedgerContextsV2.txInInfoOutRef i == utxo token) $ LedgerContextsV2.txInfoInputs info

    checkMintedAmount :: Bool
    checkMintedAmount = case LedgerValueV1.flattenValue $ LedgerContextsV2.txInfoMint info of
        [(_, tn', amount)] -> tn' == name token && amount == 1
        _                  -> False


policy :: TokenParam -> LedgerApiV2.MintingPolicy
policy token = LedgerApiV2.mkMintingPolicyScript $
    $$(PlutusTx.compile [|| UtilsTypedScriptsMintingV2.mkUntypedMintingPolicy . mkPolicy ||])
    `PlutusTx.applyCode`
    PlutusTx.liftCode token


-- curSymbol :: SignParam -> Ledger.CurrencySymbol
-- curSymbol = UtilsScriptsV2.scriptCurrencySymbol . policy

-- policyScript :: SignParam -> LedgerApiV2.Script
-- policyScript = LedgerApiV2.unMintingPolicyScript . policy

-- mintValidator :: LedgerApiV2.Validator
-- mintValidator = LedgerApiV2.Validator policyScript
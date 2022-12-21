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

module Minting.MintBurn where

import qualified Ledger                              as Ledger
import qualified Plutus.Script.Utils.V2.Scripts      as UtilsScriptsV2
import qualified Plutus.V2.Ledger.Api                as LedgerApiV2
import qualified Plutus.V2.Ledger.Contexts           as LedgerContextsV2
import qualified PlutusTx
import           PlutusTx.Prelude                    hiding (unless)
import qualified Plutus.V1.Ledger.Value                                as LedgerValueV1 (flattenValue)

data TokenParam = TokenParam
    {
        utxo :: LedgerApiV2.TxOutRef,
        name :: LedgerApiV2.TokenName,
        minter :: Ledger.PaymentPubKeyHash
    }

PlutusTx.unstableMakeIsData ''TokenParam
PlutusTx.makeLift ''TokenParam

data Action = Mint | Burn

PlutusTx.unstableMakeIsData ''Action

{-# INLINABLE mkPolicy #-}
mkPolicy :: TokenParam -> Action -> LedgerContextsV2.ScriptContext -> Bool
mkPolicy token action context = 
    let 
        txinfo :: LedgerContextsV2.TxInfo 
        txinfo = LedgerContextsV2.scriptContextTxInfo context

        checkTxSignedByMinter :: Bool
        checkTxSignedByMinter = LedgerContextsV2.txSignedBy (LedgerContextsV2.scriptContextTxInfo context) $ Ledger.unPaymentPubKeyHash $ minter token
    in 
        case action of 
            Mint -> 
                let
                    hasUtxo :: Bool
                    hasUtxo = any (\i -> LedgerContextsV2.txInInfoOutRef i == utxo token) $ LedgerContextsV2.txInfoInputs txinfo

                    checkMintedAmount :: Bool
                    checkMintedAmount = case LedgerValueV1.flattenValue $ LedgerContextsV2.txInfoMint txinfo of
                        [(_, tn', amount)] -> tn' == name token && amount == 1
                        _                  -> False
                    
                in
                    traceIfFalse "Utxo not equal" hasUtxo &&
                    traceIfFalse "wrong amount minted" checkMintedAmount &&
                    traceIfFalse "Not signed by minter" checkTxSignedByMinter
            Burn ->
                let 
                    checkBurnedAmount :: Bool
                    checkBurnedAmount = case LedgerValueV1.flattenValue $ LedgerContextsV2.txInfoMint txinfo of
                        [(_, tn', amount)] -> tn' == name token && amount == -1
                        _                  -> False
                in 
                    traceIfFalse "wrong amount minted" checkBurnedAmount &&
                    traceIfFalse "Not signed by minter" checkTxSignedByMinter

wrappedPolicy :: TokenParam -> BuiltinData -> BuiltinData -> ()
wrappedPolicy param r c = check (mkPolicy param (PlutusTx.unsafeFromBuiltinData r) (PlutusTx.unsafeFromBuiltinData c))

policy :: TokenParam -> LedgerApiV2.MintingPolicy
policy tokenparam = LedgerApiV2.mkMintingPolicyScript $
    $$(PlutusTx.compile [|| wrappedPolicy ||])
    `PlutusTx.applyCode`
    PlutusTx.liftCode tokenparam

curSymbol :: TokenParam -> Ledger.CurrencySymbol
curSymbol = UtilsScriptsV2.scriptCurrencySymbol . policy

-- policyScript :: LedgerApiV2.Script 
-- policyScript = LedgerApiV2.unMintingPolicyScript policy 

-- mintValidator :: LedgerApiV2.Validator 
-- mintValidator = LedgerApiV2.Validator policyScript

-- scriptHash :: LedgerApiV2.ScriptHash
-- scriptHash = UtilsScriptsV2.scriptHash policyScript

-- validatorHash :: UtilsScriptsV2.ValidatorHash
-- validatorHash = UtilsScriptsV2.validatorHash mintValidator

-- mintingPolicyHash :: UtilsScriptsV2.MintingPolicyHash
-- mintingPolicyHash = UtilsScriptsV2.mintingPolicyHash policy


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

module Minting.OffChainMintBurn where

--Import Externos

import qualified Control.Monad                       as Monad (void)
import qualified Data.Aeson                          as DataAeson (ToJSON, FromJSON)
import qualified Data.Text                           as DataText (Text)
import qualified Data.Void                           as DataVoid (Void)
import qualified GHC.Generics                        as GHCGenerics (Generic)
import qualified Ledger                              (getCardanoTxId)
import qualified Ledger.Constraints                  as LedgerConstraints
import qualified Plutus.Contract                     as PlutusContract
import qualified Plutus.V2.Ledger.Api              as LedgerApiV2
import           PlutusTx.Prelude                    hiding (unless)
import qualified Prelude                             as P 
import qualified Schema                              (ToSchema)
import qualified Text.Printf                         as TextPrintf (printf)
import qualified Plutus.V1.Ledger.Scripts as ScriptsLedger
import qualified Data.Map               as Map
import qualified PlutusTx

import qualified Minting.MintBurn as OnChain

data MintParams = MintParams
    { 
        mpAddress :: LedgerApiV2.Address, 
        mpTokenName    :: LedgerApiV2.TokenName,
        mpQuantity :: Integer
    } deriving (P.Show, GHCGenerics.Generic, DataAeson.ToJSON, DataAeson.FromJSON, Schema.ToSchema)

type MintSchema = PlutusContract.Endpoint "mint" MintParams

mint :: MintParams -> PlutusContract.Contract w MintSchema DataText.Text ()
mint mp = do
        utxos <- PlutusContract.utxosAt $ mpAddress mp
        case Map.keys utxos of
            [] -> PlutusContract.logInfo @P.String $ TextPrintf.printf "No Utxos found"
            (oref:_) -> do
                let 
                    param = OnChain.TokenParam
                            {
                                OnChain.utxo = oref,
                                OnChain.name = mpTokenName mp
                            }
                    val     = LedgerApiV2.singleton (OnChain.curSymbol param) (mpTokenName mp) (mpQuantity mp)
                    lookups = LedgerConstraints.plutusV2MintingPolicy (OnChain.policy param) P.<>
                            LedgerConstraints.unspentOutputs utxos
                    tx      = LedgerConstraints.mustMintValue val
                ledgerTx <- PlutusContract.submitTxConstraintsWith @DataVoid.Void lookups tx
                Monad.void $ PlutusContract.awaitTxConfirmed $ Ledger.getCardanoTxId ledgerTx
                PlutusContract.logInfo @P.String $ TextPrintf.printf "forged %s" (P.show val)


endpoints :: PlutusContract.Contract () MintSchema DataText.Text ()
endpoints = mint' >> endpoints
  where
    mint' = PlutusContract.awaitPromise $ PlutusContract.endpoint @"mint" mint
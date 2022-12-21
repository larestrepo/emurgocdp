{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE OverloadedStrings   #-}

module Minting.TraceMintBurn where

import qualified Control.Monad                       as Monad (void)
import qualified Wallet.Emulator.Wallet as WalletEmulator 
import qualified Plutus.Trace.Emulator as TraceEmulator

import qualified Minting.OffChainMintBurn as OffChain

test ::  IO ()
test = TraceEmulator.runEmulatorTraceIO $ do
    let tn = "Test"
    h1 <- TraceEmulator.activateContractWallet (WalletEmulator.knownWallet 1) OffChain.endpoints
    h2 <- TraceEmulator.activateContractWallet (WalletEmulator.knownWallet 2) OffChain.endpoints
    h3 <- TraceEmulator.activateContractWallet (WalletEmulator.knownWallet 3) OffChain.endpoints
    TraceEmulator.callEndpoint @"mint" h1 $ OffChain.MintParams
        {
            OffChain.mpAddress = WalletEmulator.mockWalletAddress (WalletEmulator.knownWallet 1), 
            OffChain.mpTokenName = tn,
            OffChain.mpQuantity = 1
        }
    Monad.void $ TraceEmulator.waitUntilSlot 5

    TraceEmulator.callEndpoint @"mint" h2 $ OffChain.MintParams
        {
            OffChain.mpAddress = WalletEmulator.mockWalletAddress (WalletEmulator.knownWallet 2), 
            OffChain.mpTokenName = tn,
            OffChain.mpQuantity = 1
        }
    Monad.void $ TraceEmulator.waitNSlots 3

    TraceEmulator.callEndpoint @"mint" h3 $ OffChain.MintParams
        {
            OffChain.mpAddress = WalletEmulator.mockWalletAddress (WalletEmulator.knownWallet 3), 
            OffChain.mpTokenName = tn,
            OffChain.mpQuantity = 1
        }
    Monad.void $ TraceEmulator.waitNSlots 1

{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DataKinds        #-}

module Simple.EmulatorSimpleType where

import Control.Monad.Freer.Extras as Extras
import Data.Functor               (void)

import Plutus.Trace
import Wallet.Emulator.Wallet
import qualified Plutus.Trace.Emulator as Emulator

import qualified Simple.OffChainType as OffChain


test :: IO ()
test = Emulator.runEmulatorTraceIO myTrace 

myTrace :: Emulator.EmulatorTrace ()
myTrace = do 
    -- Send x amount of ADa into the contract
    h1 <- Emulator.activateContractWallet (knownWallet 1) OffChain.endpoints
    h2 <- Emulator.activateContractWallet (knownWallet 2) OffChain.endpoints
    h3 <- Emulator.activateContractWallet (knownWallet 3) OffChain.endpoints
    Emulator.callEndpoint @"produce" h1 30000000
    void $ waitUntilSlot 20
    Emulator.callEndpoint @"consume" h2 200
    void $ waitUntilSlot 20
    Emulator.callEndpoint @"consume" h3 100
    s <- waitNSlots 2
    Extras.logInfo $ "reached " ++ show s
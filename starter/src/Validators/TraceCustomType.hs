
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DataKinds        #-}

module Validators.TraceCustomType where

--Plutus modules
import qualified Plutus.Trace.Emulator as Emulator
import Control.Monad.Freer.Extras as Extras
import Data.Functor               (void)
import Plutus.Trace
import Wallet.Emulator.Wallet

-- Our offchain code
import qualified Validators.CustomTypeOffChain as OffChain


test :: IO ()
test = Emulator.runEmulatorTraceIO trace1


trace1 :: Emulator.EmulatorTrace ()
trace1 = do
    h1 <- Emulator.activateContractWallet (knownWallet 1) OffChain.endpoints
    h2 <- Emulator.activateContractWallet (knownWallet 2) OffChain.endpoints

    Emulator.callEndpoint @"give" h1 20000000
    void $ waitUntilSlot 20
    Emulator.callEndpoint @"grab" h2 100
    void $ waitNSlots 2
    Emulator.callEndpoint @"grab" h2 300
    s <- waitNSlots 2
    Extras.logInfo $ "reached " ++ show s





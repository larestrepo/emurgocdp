{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DataKinds        #-}

module Beneficiary.EmulatorBeneficiary where

import Control.Monad.Freer.Extras as Extras
import Data.Functor               (void)

import Plutus.Trace
import Wallet.Emulator.Wallet as Wallet
import Data.Default               (Default (..))
import qualified Plutus.Trace.Emulator as Emulator
import Ledger.TimeSlot as TimeSlot

import qualified Beneficiary.OffChain as OffChain


test :: IO ()
test = Emulator.runEmulatorTraceIO myTrace 

myTrace :: Emulator.EmulatorTrace ()
myTrace = do 
    -- Send x amount of ADa into the contract
    h1 <- Emulator.activateContractWallet (knownWallet 1) OffChain.endpoints
    h2 <- Emulator.activateContractWallet (knownWallet 2) OffChain.endpoints
    h3 <- Emulator.activateContractWallet (knownWallet 3) OffChain.endpoints
    Emulator.callEndpoint @"produce" h1 $ OffChain.ProduceParams {
        OffChain.ppBeneficiary = Wallet.mockWalletPaymentPubKeyHash $ knownWallet 2
        , OffChain.ppDeadline = TimeSlot.slotToBeginPOSIXTime def 20
        , OffChain.ppGuess = 3000
        , OffChain.ppAmount = 30000000
    }
    -- 1. wrong beneficiary
    --2. deadline not passed
    --3. wrong guess
    void $ waitUntilSlot 2
    Emulator.callEndpoint @"consume" h2 $ OffChain.ConsumeParams {
        OffChain.getRedeem = 3000
    }
    void $ waitUntilSlot 20
    Emulator.callEndpoint @"consume" h3 $ OffChain.ConsumeParams {
        OffChain.getRedeem = 3000
    }
    void $ waitUntilSlot 1
    Emulator.callEndpoint @"consume" h2 $  OffChain.ConsumeParams {
        OffChain.getRedeem = 20000
    }
    void $ waitUntilSlot 1
    Emulator.callEndpoint @"consume" h2 $  OffChain.ConsumeParams {
        OffChain.getRedeem = 3000
    }
    s <- waitNSlots 1
    Extras.logInfo $ "reached " ++ show s
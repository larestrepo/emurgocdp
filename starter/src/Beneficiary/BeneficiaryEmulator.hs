
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DataKinds        #-}

module Beneficiary.BeneficiaryEmulator where

--Plutus modules
import qualified Plutus.Trace.Emulator as Emulator
import Data.Default               (Default (..))
import Control.Monad.Freer.Extras as Extras
import Data.Functor               (void)
import Plutus.Trace
import qualified Wallet.Emulator.Wallet as Wallet
import qualified Ledger.TimeSlot as TimeSlot

-- Our offchain code
import qualified Beneficiary.BeneficiaryOffChain as OffChain


test :: IO ()
test = Emulator.runEmulatorTraceIO trace1


trace1 :: Emulator.EmulatorTrace ()
trace1 = do
    h1 <- Emulator.activateContractWallet (Wallet.knownWallet 1) OffChain.endpoints
    h2 <- Emulator.activateContractWallet (Wallet.knownWallet 2) OffChain.endpoints
    h3 <- Emulator.activateContractWallet (Wallet.knownWallet 3) OffChain.endpoints

    Emulator.callEndpoint @"start" h1 $ OffChain.StartParams {
        OffChain.spCreator = Wallet.mockWalletPaymentPubKeyHash $ Wallet.knownWallet 3,
        OffChain.spBeneficiary = Wallet.mockWalletPaymentPubKeyHash $ Wallet.knownWallet 2,
        OffChain.spDeadline = TimeSlot.slotToBeginPOSIXTime def 30,
        OffChain.spGuess = 20,
        OffChain.spAmount = 40000000
    }
    void $ waitNSlots 2
    Emulator.callEndpoint @"grab" h3 $ OffChain.GrabParams {
        OffChain.grabRedeem = 20
    }
    void $ waitNSlots 2
    Emulator.callEndpoint @"grab" h2 $ OffChain.GrabParams {
        OffChain.grabRedeem = 30
    }
    void $ waitNSlots 40
    Emulator.callEndpoint @"grab" h2 $ OffChain.GrabParams {
        OffChain.grabRedeem = 20
    }
    s <- waitNSlots 2
    Extras.logInfo $ "reached " ++ show s





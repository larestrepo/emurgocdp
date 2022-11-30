{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DataKinds        #-}

module Spec.EmulatorSimpleType where

import Control.Monad.Freer.Extras as Extras
import Data.Default               (Default (..))
import Data.Functor               (void)
import Test.Tasty as Tasty

import Ledger.TimeSlot
import Plutus.Trace
import Wallet.Emulator.Wallet
import qualified Plutus.Trace.Emulator as Emulator
import qualified Plutus.Contract.Test as ContractTest
import qualified Wallet.Emulator.Folds as WalletFolds
import qualified Ledger.Ada as Ada

import qualified Simple.OffChainType as OffChain
import qualified Simple.SimpleType as OnChain


test :: Tasty.TestTree
test = 
    testGroup "description"
    [ContractTest.checkPredicate "run myTrace definition"
    (ContractTest.walletFundsChange (knownWallet 1) (Ada.adaValueOf (-30))
     ContractTest..&&. ContractTest.walletFundsChange (knownWallet 3) (Ada.adaValueOf 30)
    ContractTest..&&. ContractTest.valueAtAddress OnChain.simpleAddress (Ada.adaValueOf 0 ==))
    myTrace
    ]

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
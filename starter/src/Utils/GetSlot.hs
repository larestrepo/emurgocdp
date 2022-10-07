module Getslot where

import Ledger
import Ledger.TimeSlot
--references
-- https://cardano.stackexchange.com/questions/6955/when-was-the-slot-duration-set-to-1-sec-on-cardano-testnet
-- https://www.bitcompiler.com/datetime-to-milliseconds

testnetdefault :: SlotConfig
testnetdefault = SlotConfig 1000 $ POSIXTime 1660003200000

slotWhenChangedto1Sec :: Slot
-- slotWhenChangedto1Sec = 1598400
slotWhenChangedto1Sec = 0



getSlot :: POSIXTime -> Slot
getSlot time = slotWhenChangedto1Sec + posixTimeToEnclosingSlot testnetdefault time
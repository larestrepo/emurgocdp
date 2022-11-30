
module SlotConversion where 

import Ledger.TimeSlot
import Ledger

testnetSlotConfig :: SlotConfig 
testnetSlotConfig = SlotConfig 1000 $ POSIXTime 1660003200000

getSlot :: POSIXTime -> Slot
getSlot n = posixTimeToEnclosingSlot testnetSlotConfig n 
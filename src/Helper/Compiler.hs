module Helper.Compiler where

-- Haskell libraries
import Codec.Serialise (serialise)
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Short as SBS
import qualified Data.ByteString as B
import qualified Data.ByteString.Base16 as Base16

-- Cardano module
import Cardano.Api.Shelley (PlutusScript (..), PlutusScriptV2)

--Plutus
import qualified Plutus.V2.Ledger.Api                            as LedgerApiV2
import qualified Cardano.Binary as CBOR

import qualified Simple.Simple as Simple

script :: LedgerApiV2.Script
script = LedgerApiV2.unValidatorScript Simple.validator

scriptShortBs :: SBS.ShortByteString
scriptShortBs = (SBS.toShort . LBS.toStrict . serialise) script

srlScript :: PlutusScript PlutusScriptV2 
srlScript = PlutusScriptSerialised scriptShortBs

scriptCBORHex :: B.ByteString 
scriptCBORHex = Base16.encode $ CBOR.serialize' srlScript


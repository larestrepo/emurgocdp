module Utils.Compiler where

-- Haskell imports
import Codec.Serialise (serialise)
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Short as SBS
import qualified Data.ByteString.Base16 as Base16
import qualified Data.ByteString as B

--Plutus imports
import qualified Plutus.V2.Ledger.Api                            as LedgerApiV2
import qualified Cardano.Binary as CBOR
import Cardano.Api.Shelley (PlutusScript (..), PlutusScriptV2)

--my module imports
import qualified Validators.SimpleType as Val

script :: LedgerApiV2.Script
script = LedgerApiV2.unValidatorScript Val.validator

scriptBs :: SBS.ShortByteString
scriptBs = (SBS.toShort . LBS.toStrict . serialise) script

srlScript :: PlutusScript PlutusScriptV2
srlScript = PlutusScriptSerialised scriptBs

scriptCBORHex :: B.ByteString
scriptCBORHex = Base16.encode $ CBOR.serialize' srlScript
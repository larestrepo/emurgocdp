{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Starting.MyCompiler where

import Cardano.Api
import Cardano.Api.Shelley (PlutusScript (..))
import Codec.Serialise (serialise)
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Short as SBS
import qualified Plutus.V2.Ledger.Api                            as LedgerApiV2
import qualified Validators.SimpleType as MFS


writeValidator :: FilePath -> LedgerApiV2.Validator -> IO (Either (FileError ()) ())
writeValidator file = writeFileTextEnvelope @(PlutusScript PlutusScriptV2) file Nothing . PlutusScriptSerialised . SBS.toShort . LBS.toStrict . serialise . LedgerApiV2.unValidatorScript

writeMyFirstValidatorScript :: IO (Either (FileError ()) ())
writeMyFirstValidatorScript = writeValidator "src/Validators/SimpleType.plutus" MFS.validator
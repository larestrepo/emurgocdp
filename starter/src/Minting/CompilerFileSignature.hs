{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Minting.CompilerFile where

import           Cardano.Api
import           Cardano.Api.Shelley (PlutusScript (..))
import           Codec.Serialise (serialise)
import qualified Data.Aeson                          as DataAeson
import qualified Data.ByteString.Lazy                as LBS
import qualified Data.ByteString.Short               as SBS
import qualified Plutus.V2.Ledger.Api                as LedgerApiV2
import qualified PlutusTx
import qualified Ledger

import qualified Minting.OnChain as OnChain

main :: IO()
main = do 
    writeInitDatum
    _ <- writeMyFirstValidatorScript

    return ()

dataToScriptData :: LedgerApiV2.Data -> ScriptData
dataToScriptData (LedgerApiV2.Constr n xs) = ScriptDataConstructor n $ dataToScriptData <$> xs
dataToScriptData (LedgerApiV2.Map xs)      = ScriptDataMap [(dataToScriptData x, dataToScriptData y) | (x, y) <- xs]
dataToScriptData (LedgerApiV2.List xs)     = ScriptDataList $ dataToScriptData <$> xs
dataToScriptData (LedgerApiV2.I n)         = ScriptDataNumber n
dataToScriptData (LedgerApiV2.B bs)        = ScriptDataBytes bs

writeJSON :: PlutusTx.ToData a => FilePath -> a -> IO ()
writeJSON file = LBS.writeFile file . DataAeson.encode . scriptDataToJson ScriptDataJsonDetailedSchema . dataToScriptData . PlutusTx.toData

writeInitDatum :: IO ()
writeInitDatum = writeJSON "src/Minting/Deploy/unit.json" ()

writeValidator :: FilePath -> LedgerApiV2.MintingPolicy -> IO (Either (FileError ()) ())
writeValidator file = writeFileTextEnvelope @(PlutusScript PlutusScriptV2) file Nothing . PlutusScriptSerialised . SBS.toShort . LBS.toStrict . serialise . LedgerApiV2.unMintingPolicyScript

writeMyFirstValidatorScript :: IO (Either (FileError ()) ())
writeMyFirstValidatorScript = writeValidator "src/Minting/Deploy/Minting.plutus" $ OnChain.policy $ OnChain.SignParam
    {
        OnChain.beneficiary = Ledger.PaymentPubKeyHash "75eacb8808f937e42cde4312d2d4bb42bd1cbfca379bbe90a3ec0383"
    }
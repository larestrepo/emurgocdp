--1 Extensions
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DataKinds #-}

--This is to work not only with Strings
{-# LANGUAGE OverloadedStrings   #-}

-- required to use custom data types
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE TypeOperators       #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE MultiParamTypeClasses      #-} 


module Parameterized.ParameterOnChain where
-- Sections of a Plutus contract


--2 Imports
import PlutusTx
import PlutusTx.Prelude
import qualified Ledger.Address                                  as V1LAddress
import qualified Plutus.V2.Ledger.Api                            as V2LedgerApi
import qualified Plutus.V2.Ledger.Contexts                       as Contexts
import qualified Plutus.V1.Ledger.Value                          as ValueV1
import qualified Plutus.Script.Utils.V2.Typed.Scripts.Validators as V2UtilsTypeScripts
import qualified Prelude                                         as P
import qualified Ledger                                          (PaymentPubKeyHash, unPaymentPubKeyHash, AssetClass)
import qualified Plutus.V1.Ledger.Interval                            as LedgerIntervalV1
import qualified Ledger.Ada                                      as Ada

--3 Onchain code

type NFT = Ledger.AssetClass

data Dat = Dat
    {
    dSeller :: Ledger.PaymentPubKeyHash,
    dNFT :: NFT,
    dPrice :: Integer
    } deriving P.Show

instance Eq Dat where 
    {-# INLINABLE (==) #-}
    a == b = (dSeller a == dSeller b) &&
             (dPrice a == dPrice b) &&
             (dNFT a == dNFT b)

PlutusTx.unstableMakeIsData ''Dat
PlutusTx.makeLift ''Dat

data Redeem = SellerGetBackNFT | BuyNFT
    deriving P.Show

PlutusTx.unstableMakeIsData ''Redeem -- This is to instantiate the IsData class
PlutusTx.makeLift ''Redeem

{-# INLINABLE mainValidator #-}
--Actual validator logic
mainValidator :: Dat -> Redeem -> Contexts.ScriptContext -> Bool
mainValidator d r context = 
    case r of
        SellerGetBackNFT -> validateGetBackAction d context
        BuyNFT -> validateBuyAction d context

{-# INLINABLE validateGetBackAction #-}
validateGetBackAction :: Dat -> Contexts.ScriptContext -> Bool
validateGetBackAction d context = traceIfFalse "Only the seller can get back the NFT" signedBySeller
    where 
        txinfo :: Contexts.TxInfo
        txinfo = Contexts.scriptContextTxInfo context
        
        signedBySeller :: Bool
        signedBySeller = Contexts.txSignedBy txinfo $ Ledger.unPaymentPubKeyHash (dSeller d)


{-# INLINABLE validateBuyAction #-}
validateBuyAction :: Dat -> Contexts.ScriptContext -> Bool
validateBuyAction d context = traceIfFalse "Amount sent less than the NFT price" checkIfPaymentIsCorrect
    where 
        txinfo :: Contexts.TxInfo
        txinfo = Contexts.scriptContextTxInfo context

        checkIfPaymentIsCorrect :: Bool
        checkIfPaymentIsCorrect =
            let 
                valuepay = Contexts.valuePaidTo txinfo $ Ledger.unPaymentPubKeyHash (dSeller d)
            in
                valuepay `ValueV1.gt` (Ada.lovelaceValueOf $ dPrice d)

data Market
instance V2UtilsTypeScripts.ValidatorTypes Market where
    type instance RedeemerType Market = Redeem
    type instance DatumType Market = Dat


--Boilerplate
marketTypeV :: V2UtilsTypeScripts.TypedValidator Market
marketTypeV = V2UtilsTypeScripts.mkTypedValidator @Market 
    $$(compile [|| mainValidator ||])
    $$(compile [|| wrap ||]) where
        wrap = V2UtilsTypeScripts.mkUntypedValidator @Dat @Redeem

validator :: V2LedgerApi.Validator
validator = V2UtilsTypeScripts.validatorScript marketTypeV

validatorHash :: V2LedgerApi.ValidatorHash
validatorHash = V2UtilsTypeScripts.validatorHash marketTypeV
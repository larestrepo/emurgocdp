
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE ScopedTypeVariables #-}

{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE TypeOperators       #-}
{-# LANGUAGE OverloadedStrings   #-}


module Simple.OffChainType where

-- Haskell imports
import qualified Control.Monad            as Monad (void)
import qualified Data.Map                 as Map

-- Plutus imports

import qualified Plutus.Contract          as PlutusContract
import qualified Prelude                  as P
import qualified Text.Printf              as TextPrintf (printf)
import           PlutusTx.Prelude
import           Ledger.Ada               as Ada
import qualified Ledger.Constraints       as Constraints
import qualified Ledger.Tx                as LedgerTx
import qualified Data.Void                as Void (Void)
import qualified PlutusTx
import qualified Plutus.V1.Ledger.Scripts as ScriptsLedger
import qualified Data.Text                as DT (Text)


import qualified Simple.SimpleType as OnChain

-- Contract Monad
-- Contract w s e a 

-- w: Pass information 
-- s: endpoints 
-- e: errors
-- a: result

-- dummy :: Contract () EmptySchema Text ()
-- dummy = logInfo @String "Producing our first utxo in our SimpleType contract"

type SimpleTypeSchema = 
    PlutusContract.Endpoint "produce" Integer 
    PlutusContract..\/ PlutusContract.Endpoint "consume" Integer

produce :: forall w s e. PlutusContract.AsContractError e => Integer -> PlutusContract.Contract w s e ()
produce value = do 
    PlutusContract.logInfo @P.String "------------------------produce start initialize------------------------"
    let tx = Constraints.mustPayToOtherScript (OnChain.simpleHash) (ScriptsLedger.Datum $ PlutusTx.toBuiltinData ()) (lovelaceValueOf value)
        lookups = Constraints.plutusV2OtherScript OnChain.validator

    submittedTx <- PlutusContract.submitTxConstraintsWith @Void.Void lookups tx
    Monad.void $ PlutusContract.awaitTxConfirmed $ LedgerTx.getCardanoTxId submittedTx
    PlutusContract.logInfo @P.String "------------------------Transaction processed------------------------"
        

consume :: forall w s e. PlutusContract.AsContractError e => Integer -> PlutusContract.Contract w s e ()
consume redeem = do 
    if redeem == 100
        then do 
            utxos <- PlutusContract.utxosAt OnChain.simpleAddress
            let orefs = fst <$> Map.toList utxos
                lookups = Constraints.unspentOutputs utxos P.<>
                        Constraints.plutusV2OtherScript OnChain.validator
                tx :: Constraints.TxConstraints Void.Void Void.Void
                tx      = mconcat [Constraints.mustSpendScriptOutput oref $ ScriptsLedger.Redeemer $ PlutusTx.toBuiltinData redeem | oref <- orefs]
            submittedTx <- PlutusContract.submitTxConstraintsWith @Void.Void lookups tx
            Monad.void $ PlutusContract.awaitTxConfirmed $ LedgerTx.getCardanoTxId submittedTx
            PlutusContract.logInfo @P.String "Correct guess "

        else PlutusContract.logInfo @P.String "Not correct"


endpoints :: PlutusContract.Contract () SimpleTypeSchema DT.Text ()
endpoints = PlutusContract.awaitPromise  (produce' `PlutusContract.select` consume') >> endpoints
    where 
        produce' = PlutusContract.endpoint @"produce" produce
        consume' = PlutusContract.endpoint @"consume" consume
#!/usr/bin/env bash
set -e
set -o pipefail

set -x

source helpers.sh

read -p 'Script name to reference: ' SCRIPT_NAME

echo $SCRIPT_NAME

if [[ $SCRIPT_NAME != \addr_* ]];
then 
    SCRIPT_ADDRESS=$($CARDANO_CLI address build --payment-script-file $WORK/plutus-scripts/${SCRIPT_NAME}.plutus --testnet-magic $TESTNET_MAGIC)
    mkdir -p $BASE/.priv/wallets/${SCRIPT_NAME}
    echo $SCRIPT_ADDRESS > $BASE/.priv/wallets/${SCRIPT_NAME}/${SCRIPT_NAME}.payment.addr
else
    SCRIPT_ADDRESS=$SCRIPT_NAME
fi

read -p 'Select the key witness wallet name: ' WITNESS
getInputTx ${WITNESS}
WITNESS_TX=$SELECTED_UTXO
WITNESS_ADDR=$SELECTED_WALLET_ADDR
WITNESS_NAME=${SELECTED_WALLET_NAME}
read -p 'lovelace to cover the transaction and the reference script: ' LOVELACE_TO_SEND

# Validate the transaction
txOutRefId=${WITNESS_TX::-2}
echo txOutRefId

SCRIPT_FILE=$WORK/plutus-scripts/${SCRIPT_NAME}.plutus 

$CARDANO_CLI transaction build \
--tx-in ${WITNESS_TX} \
--tx-out ${WITNESS_ADDR}+${LOVELACE_TO_SEND} \
--tx-out-reference-script-file ${SCRIPT_FILE} \
--change-address=${WITNESS_ADDR} \
--testnet-magic ${TESTNET_MAGIC}  \
--out-file $WORK/transactions/tx.draft \
--babbage-era

read -p 'Sign and submit creation of reference script? [Y/N]: ' input

case $input in
        [yY][eE][sS]|[yY])
            echo "You say Yes"
            $CARDANO_CLI transaction sign \
            --tx-body-file $WORK/transactions/tx.draft \
            --signing-key-file $BASE/.priv/wallets/${WITNESS_NAME}/${WITNESS_NAME}.payment.skey \
            --testnet-magic $TESTNET_MAGIC \
            --out-file $WORK/transactions/tx.signed

            $CARDANO_CLI transaction submit --tx-file $WORK/transactions/tx.signed --testnet-magic $TESTNET_MAGIC

            ;;
        [nN][oO]|[nN])
            echo "You say No"
            ;;
        *)
            echo "Invalid input..."
            exit 1
            ;;
esac

#!/usr/bin/env bash

set -e
set -o pipefail

source helpers.sh

set -x

read -p 'Script name to spend from: ' SCRIPT_NAME

SCRIPT_FILE=$WORK/plutus-scripts/${SCRIPT_NAME}.plutus 
SCRIPT_ADDRESS=$($CARDANO_CLI address build --payment-script-file $SCRIPT_FILE --testnet-magic $TESTNET_MAGIC)
mkdir -p $BASE/.priv/wallets/${SCRIPT_NAME}
echo $SCRIPT_ADDRESS > $BASE/.priv/wallets/${SCRIPT_NAME}/${SCRIPT_NAME}.payment.addr

read -p 'Receiving wallet name: ' TO_WALLET_NAME

if [[ $TO_WALLET_NAME != \addr_* ]];
then 
    TO_WALLET_ADDRESS=$(cat $BASE/.priv/wallets/$TO_WALLET_NAME/$TO_WALLET_NAME.payment.addr)
else
    TO_WALLET_ADDRESS=$TO_WALLET_NAME
fi

section "Select Script UTxO"
getInputTx ${SCRIPT_NAME}
SCRIPT_UTXO=$SELECTED_UTXO
PAYMENT=$SELECTED_UTXO_LOVELACE

section "Select Collateral UTxO"
read -p 'Collateral wallet name: ' COLLATERAL
getInputTx ${COLLATERAL}
COLLATERAL_TX=$SELECTED_UTXO
FEE_ADDR=$SELECTED_WALLET_ADDR

read -p 'Datum hash file name: ' DATUM_HASH_FILE
read -p 'Redeemer file name: ' REDEEMER_FILE

REQUIRED_SIGNER_ARRAY=()
SIGNING_KEY_FILE_ARRAY=()
while true; do
read -p 'Add required-signer-hash? [Y/N]: ' input
case $input in
    [yY][eE][sS]|[yY])
        echo "You say Yes"
        read -p 'Input required-signer-hash: ' REQUIRED_SIGNER
        read -p 'Input path to skey: ' SIGNING_KEY_FILE
        REQUIRED_SIGNER_ARRAY+='--required-signer-hash '
        REQUIRED_SIGNER_ARRAY+=$REQUIRED_SIGNER
        REQUIRED_SIGNER_ARRAY+=' '
        SIGNING_KEY_FILE_ARRAY+='--signing-key-file '
        SIGNING_KEY_FILE_ARRAY+=$SIGNING_KEY_FILE
        SIGNING_KEY_FILE_ARRAY+=' '
        ;;
    [nN][oO]|[nN])
        echo "You say No"
        break
        ;;
    *)
        echo "Invalid input..."
        exit 1
        ;;
esac
done

INVALID_BEFORE_ARRAY=()
INVALID_HEREAFTER_ARRAY=()
read -p 'Is the script constraint by deadline or time? [Y/N]: ' input
case $input in
    [yY][eE][sS]|[yY])
        ./currentSlot.sh
        echo "You say Yes"
        echo "[X, X+epochs_valid) validity range in slots"
        read -p 'Input the starting validity slot number (X): ' VALIDITY
        # echo 'Current epoch is: ' 
        read -p 'Input the number of epochs for validity (i.e current slot + 200): ' EPOCHS_VALID
        INVALID_BEFORE_ARRAY+='--invalid-before '
        INVALID_BEFORE_ARRAY+=$((VALIDITY))
        INVALID_BEFORE_ARRAY+=' '
        INVALID_HEREAFTER_ARRAY+='--invalid-hereafter '
        INVALID_HEREAFTER_ARRAY+=$((EPOCHS_VALID))
        INVALID_HEREAFTER_ARRAY+=' '
        ;;
    [nN][oO]|[nN])
        echo "You say No"
        break
        ;;
    *)
        echo "Invalid input..."
        exit 1
        ;;
esac

# Check if wanted to add additional outputs


TO_WALLET_NAME_ARRAY=()
while true; do
read -p 'Do you want to add additional outputs? [Y/N]: ' input
case $input in
    [yY][eE][sS]|[yY])
        echo "You say Yes"
        read -p 'Lovelace to send: ' LOVELACE_TO_SEND
        read -p 'Receiving wallet name: ' TO_WALLET_NAME

        echo TO_WALLET_NAME

        if [[ $TO_WALLET_NAME != \addr_* ]];
        then 
            TO_WALLET_ADDRESS=$(cat $BASE/.priv/wallets/$TO_WALLET_NAME/$TO_WALLET_NAME.payment.addr)
        else
            TO_WALLET_ADDRESS=$TO_WALLET_NAME
        fi
        TO_WALLET_NAME_ARRAY+='--tx-out '
        TO_WALLET_NAME_ARRAY+=$TO_WALLET_ADDRESS+$LOVELACE_TO_SEND
        PAYMENT=$(expr $PAYMENT - $LOVELACE_TO_SEND)
        ;;
    [nN][oO]|[nN])
        echo "You say No"
        break
        ;;
    *)
        echo "Invalid input..."
        exit 1
        ;;
esac
done

$CARDANO_CLI query protocol-parameters --testnet-magic $TESTNET_MAGIC > $WORK/transactions/pparams.json

#Section to allow the new feature for reference scripts

read -p 'Is the script existing in a reference utxo? [Y/N]: ' input
case $input in
    [yY][eE][sS]|[yY])
        ./currentSlot.sh
        echo "You say Yes"
        # echo 'Current epoch is: ' 
        read -p 'Witness wallet name: ' WITNESS
        getInputTx ${WITNESS}
        WITNESS_TX=$SELECTED_UTXO
        # WITNESS_ADDR=$SELECTED_WALLET_ADDR
        # WITNESS_NAME=${SELECTED_WALLET_NAME}

        build=($CARDANO_CLI transaction build \
        --babbage-era \
        --cardano-mode \
        --testnet-magic $TESTNET_MAGIC \
        ${INVALID_BEFORE_ARRAY} ${INVALID_HEREAFTER_ARRAY} \
        --change-address=${FEE_ADDR} \
        --tx-in ${SCRIPT_UTXO} \
        --spending-tx-in-reference ${WITNESS_TX} \
        --spending-plutus-script-v2 \
        --spending-reference-tx-in-datum-file $WORK/plutus-scripts/${DATUM_HASH_FILE} \
        --spending-reference-tx-in-redeemer-file $WORK/plutus-scripts/${REDEEMER_FILE} \
        --tx-in ${COLLATERAL_TX} \
        --tx-in-collateral=${COLLATERAL_TX} \
        --tx-out ${TO_WALLET_ADDRESS}+${PAYMENT} \
        ${TO_WALLET_NAME_ARRAY} \
        ${REQUIRED_SIGNER_ARRAY} \
        --protocol-params-file $WORK/transactions/pparams.json \
        --out-file $WORK/transactions/tx.draft)
        ;;
    [nN][oO]|[nN])
        build=($CARDANO_CLI transaction build \
        --babbage-era \
        --cardano-mode \
        --testnet-magic $TESTNET_MAGIC \
        ${INVALID_BEFORE_ARRAY} ${INVALID_HEREAFTER_ARRAY} \
        --change-address=${FEE_ADDR} \
        --tx-in ${SCRIPT_UTXO} \
        --tx-in-script-file ${SCRIPT_FILE} \
        --tx-in-datum-file $WORK/plutus-scripts/${DATUM_HASH_FILE} \
        --tx-in-redeemer-file $WORK/plutus-scripts/${REDEEMER_FILE} \
        --tx-in ${COLLATERAL_TX} \
        --tx-in-collateral=${COLLATERAL_TX} \
        --tx-out ${TO_WALLET_ADDRESS}+${PAYMENT} \
        ${TO_WALLET_NAME_ARRAY} \
        ${REQUIRED_SIGNER_ARRAY} \
        --protocol-params-file $WORK/transactions/pparams.json \
        --out-file $WORK/transactions/tx.draft)
        ;;
    *)
        echo "Invalid input..."
        exit 1
        ;;
esac

# print the cardano transaction build
# cat $build
# execute the cardano transaction build
"${build[@]}"

$CARDANO_CLI transaction sign \
--tx-body-file $WORK/transactions/tx.draft \
${SIGNING_KEY_FILE_ARRAY} \
--testnet-magic $TESTNET_MAGIC \
--out-file $WORK/transactions/tx.signed \

$CARDANO_CLI transaction submit --tx-file $WORK/transactions/tx.signed --testnet-magic $TESTNET_MAGIC
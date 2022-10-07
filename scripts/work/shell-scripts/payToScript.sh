#!/usr/bin/env bash
set -e
set -o pipefail

# set -x

source helpers.sh
getInputTx $1
FROM_UTXO=${SELECTED_UTXO}
FROM_WALLET_NAME=${SELECTED_WALLET_NAME}
FROM_WALLET_ADDRESS=${SELECTED_WALLET_ADDR}
FROM_BALANCE=${SELECTED_UTXO_LOVELACE}

read -p 'Lovelace to send: ' LOVELACE_TO_SEND
read -p 'Receiving script name: ' SCRIPT_NAME

echo $SCRIPT_NAME

if [[ $SCRIPT_NAME != \addr_* ]];
then 
    SCRIPT_ADDRESS=$($CARDANO_CLI address build --payment-script-file $WORK/plutus-scripts/${SCRIPT_NAME}.plutus --testnet-magic $TESTNET_MAGIC)
    mkdir -p $BASE/.priv/wallets/${SCRIPT_NAME}
    echo $SCRIPT_ADDRESS > $BASE/.priv/wallets/${SCRIPT_NAME}/${SCRIPT_NAME}.payment.addr
else
    SCRIPT_ADDRESS=$SCRIPT_NAME
fi

read -p 'Datum hash file name: ' DATUM_HASH_FILE

$CARDANO_CLI transaction build \
--tx-in ${FROM_UTXO} \
--tx-out ${SCRIPT_ADDRESS}+${LOVELACE_TO_SEND} \
--tx-out-datum-hash-file $WORK/plutus-scripts/${DATUM_HASH_FILE} \
--change-address=${FROM_WALLET_ADDRESS} \
--testnet-magic ${TESTNET_MAGIC}  \
--out-file $WORK/transactions/tx.draft \
--babbage-era

TX_HASH=$($CARDANO_CLI transaction txid --tx-body-file $WORK/transactions/tx.draft)
# TX_ANALYZE=$($CARDANO_CLI transaction view --tx-body-file $WORK/transactions/tx.draft)

echo 'Transaction with id: ' $TX_HASH

# echo 'User transaction with id: ' $TX_ANALYZE
read -p 'Sign and submit Pay to Script Tx? [Y/N]: ' input

case $input in
      [yY][eE][sS]|[yY])
            echo "You say Yes"
            $CARDANO_CLI transaction sign \
            --tx-body-file $WORK/transactions/tx.draft \
            --signing-key-file $BASE/.priv/wallets/${FROM_WALLET_NAME}/${FROM_WALLET_NAME}.payment.skey \
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

section "Creating reference script"
read -p 'Do you want to create reference script? [Y/N]: ' input

case $input in
      [yY][eE][sS]|[yY])
            echo "You say Yes"
            read -p 'Select the key witness wallet name: ' WITNESS
            getInputTx ${WITNESS}
            WITNESS_TX=$SELECTED_UTXO
            WITNESS_ADDR=$SELECTED_WALLET_ADDR
            WITNESS_NAME=${SELECTED_WALLET_NAME}
            read -p 'lovelace to cover the transaction and the reference script: ' LOVELACE_TO_SEND

            # Validate the transaction
            txOutRefId=${WITNESS_TX::-2}
            echo txOutRefId

            if [[ $TX_HASH == $txOutRefId ]];
            then 
                  SCRIPT_FILE=$WORK/plutus-scripts/${SCRIPT_NAME}.plutus 

                  $CARDANO_CLI transaction build \
                  --tx-in ${WITNESS_TX} \
                  --tx-out ${WITNESS_ADDR}+${LOVELACE_TO_SEND} \
                  --tx-out-reference-script-file ${SCRIPT_FILE} \
                  --change-address=${WITNESS_ADDR} \
                  --testnet-magic ${TESTNET_MAGIC}  \
                  --out-file $WORK/transactions/tx.draft \
                  --babbage-era
            else
                  echo "Invalid input..."
                  exit 1
            fi

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

            ;;
      [nN][oO]|[nN])
            echo "You say No"
            ;;
      *)
            echo "Invalid input..."
            exit 1
            ;;
esac


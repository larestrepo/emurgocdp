#!/usr/bin/env bash
set -e
set -o pipefail

. "$(dirname $0)"/env # soure env variables


function getInputTx() {
	BALANCE_FILE=$WORK/transactions/walletBalances.txt
	rm -f $BALANCE_FILE
	if [ -z "$1" ]
	then
		read -p 'Wallet Name: ' SELECTED_WALLET_NAME
	else
		echo 'Wallet Name: ' $1
		SELECTED_WALLET_NAME=$1
	fi
	./balance.sh $SELECTED_WALLET_NAME > $BALANCE_FILE
	SELECTED_WALLET_ADDR=$(cat $BASE/.priv/wallets/$SELECTED_WALLET_NAME/$SELECTED_WALLET_NAME.payment.addr)
	cat $BALANCE_FILE
	read -p 'TX row number starting in 1: ' TMP
	TX_ROW_NUM="$(($TMP+2))"
	TX_ROW=$(sed "${TX_ROW_NUM}q;d" $BALANCE_FILE)
	SELECTED_UTXO="$(echo $TX_ROW | awk '{ print $1 }')#$(echo $TX_ROW | awk '{ print $2 }')"
	SELECTED_UTXO_LOVELACE=$(echo $TX_ROW | awk '{ print $3 }')
	SELECTED_UTXO_TOKENS=$(echo $TX_ROW | awk '{ print $6 }')
}

walletAddress() {
	WALLET_ADDRESS=$(cat $BASE/.priv/wallets/$1/$1.payment.addr)
}

setDatumHash() {
	DATUM_HASH=$(cardano-cli transaction hash-script-data --script-data-value $DATUM_VALUE)
	#return $(cardano-cli transaction hash-script-data --script-data-value $1)
}

getScriptAddress() {
	SCRIPT_ADDRESS=$($CARDANO_CLI address build --payment-script-file $WORK/plutus-scripts/${SCRIPT_NAME}.plutus --testnet-magic $TESTNET_MAGIC)
        mkdir -p $BASE/.priv/wallets/${SCRIPT_NAME}
        echo $SCRIPT_ADDRESS > $BASE/.priv/wallets/${SCRIPT_NAME}/${SCRIPT_NAME}.payment.addr
}

function section {
  echo "============================================================================================"
  echo $1
  echo "============================================================================================"
}

function removeTxFiles() {
  rm -f tx.raw
  rm -f tx.signed
}

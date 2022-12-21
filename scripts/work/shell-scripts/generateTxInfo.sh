#!/usr/bin/env bash
set -e
set -o pipefail

if [ -z $1 ]; then
    echo "generateTxInfo.sh:  Invalid script arguments. Please provide tx id (TxId#index)"
    exit 1
fi

. "$(dirname $0)"/env # soure env variables

$CARDANO_CLI query utxo --tx-in $1 --testnet-magic $TESTNET_MAGIC --out-file $WORK/transactions/txid

echo "file stored in: " $WORK/transactions/txid
#!/usr/bin/env bash
set -e
set -o pipefail

if [ -z $1 ]; then
    echo "contractBalance.sh:  Invalid script arguments. Please provide script name as stored in ./priv/wallets"
    exit 1
fi

. "$(dirname $0)"/env # soure env variables


SCRIPT_NAME=$1
SCRIPT_ADDRESS=$($CARDANO_CLI address build --payment-script-file $WORK/plutus-scripts/${SCRIPT_NAME}.plutus --testnet-magic $TESTNET_MAGIC)
mkdir -p $BASE/.priv/wallets/${SCRIPT_NAME}
echo $SCRIPT_ADDRESS > $BASE/.priv/wallets/${SCRIPT_NAME}/${SCRIPT_NAME}.payment.addr
./balance.sh ${SCRIPT_NAME}
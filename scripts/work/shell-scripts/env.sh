#!/usr/bin/env bash

# Unofficial bash strict mode.
# See: http://redsymbol.net/articles/unofficial-bash-strict-mode/
set -e
set -o pipefail


# Make sure the cardano node socket is available 
# eg. ssh -L  /home/lawrence/src/cardano-lottery/node.socket:/home/lawrence/.cardano-testnet-node/db/node.socket cardano-server


# Define export variables
export BASE=/home/cardanodatos/emurgotemp
export WORK=$BASE/scripts/work

# PROJECT_ID is for blockfrost datum query, get your account here
# https://blockfrost.io/
# export PROJECT_ID=$(cat $BASE/scripts/cardano-cli/testnet/blockfrost.id)
export CARDANO_CLI=cardano-cli
# export CARDANO_NODE_SOCKET_PATH=/home/lawrence/src/cardano-lottery/node.socket
export TESTNET_MAGIC=2
# Loading keys
export FOR_PLUTUS_VKEY=/home/cardanodatos/emurgotemp/.priv/wallets/forPlutus/forPlutus.payment.vkey
export FOR_PLUTUS_SKEY=/home/cardanodatos/emurgotemp/.priv/wallets/forPlutus/forPlutus.payment.skey
# export PAYMENT_ADDR=


export MIN_ADA_OUTPUT_TX=2000000
export COLLATERAL_ADA=5000000
export COLLATERAL=ba8362e55ffc29eb80cf6cee1f6cb49577b48376ec3150fe85921cd5f30885f7#3



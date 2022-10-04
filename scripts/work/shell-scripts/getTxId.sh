#!/usr/bin/env bash
set -e
set -o pipefail

. "$(dirname $0)"/env # soure env variables

$CARDANO_CLI transaction txid --tx-file $WORK/transactions/tx.signed
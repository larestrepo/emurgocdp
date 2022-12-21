#!/usr/bin/env bash
set -e
set -o pipefail

. "$(dirname $0)"/env # soure env variables

$CARDANO_CLI transaction view --tx-body-file $WORK/transactions/tx.draft
#!/usr/bin/env bash

# Unofficial bash strict mode.
# See: http://redsymbol.net/articles/unofficial-bash-strict-mode/
set -e
set -o pipefail

# Enabled debug flag for bash shell
set -x

# Check if command line argument is empty or not present
if [ -z $1 ]; then
    echo "mint.sh:  Invalid script arguments"
    echo "mint.sh [preview|preprod|mainnet] [n]    where n = ticket number "
    exit 1
fi
ENV=$1

# Pull in global export variables
MY_DIR=$(dirname $(readlink -f $0))
source $MY_DIR/$ENV/env.sh

if [ "$ENV" == "mainnet" ];
then
    network="--mainnet"
else
    network="--testnet-magic $TESTNET_MAGIC"
fi


echo "Socket path: $CARDANO_NODE_SOCKET_PATH"
###############################################################

# ls -al "$CARDANO_NODE_SOCKET_PATH"
# mkdir -p $WORK
# mkdir -p $WORK-backup
# cp -f $WORK/* $WORK-backup


# Generate values from cardano-cli tool
$CARDANO_CLI query protocol-parameters $network --out-file $WORK/pparms.json
mint_script="$BASE/starting/src/Minting/Plutus/Free.plutus"
# mint_script_addr=$($CARDANO_CLI address build --payment-script-file "$mint_validator" $network)
redeemer_file_path="$WORK/redeemer.json"

# echo "starting lotto buy"

# Step 1: Get UTXOs from payment address 
mint_addr=$($CARDANO_CLI address build $network --payment-verification-key-file $FOR_PLUTUS_VKEY)
$CARDANO_CLI query utxo --address $mint_addr $network --out-file $WORK/mint-utxo.json
cat $WORK/mint-utxo.json | jq -r 'to_entries[] | select(.value.value.lovelace > '$COLLATERAL_ADA' ) | .key' > $WORK/mint-utxo-valid.json
readarray mint_utxo_valid_array < $WORK/mint-utxo-valid.json
mint_utxo_in=$(echo $mint_utxo_valid_array | tr -d '\n')

echo $mint_utxo_in

# Step 2: Get the POLICYID from the script
# $CARDANO_CLI query utxo --address $mint_script_addr $network --out-file $WORK/mint-validator-utxo.json
export POLICYID=$CARDANO_CLI transaction policyid --script-file $mint_script

# Step 3: Define destination address
destination_addr="addr_test1qpkmsauwln4hv7j2ygpky3fmmw84pn624yv9aruefetjals95yaattr99dteap4y7rqjr80ujvuguegc72rqjfs25mfsrfrjts"

echo " working with policyid=" $POLICYID

# Step 4
# Token information
token_name=$(echo -n "misegundaprueba" | xxd -p)
token_quantity=20000

echo "token name: " $token_name

$CARDANO_CLI transaction build \
--babbage-era \
--cardano-mode \
$network \
--witness-override 1 \
--tx-in $mint_utxo_in \
--tx-out $destination_addr+$MIN_ADA_OUTPUT_TX +"$token_quantity $POLICYID.$token_name" \
--tx-out addr_test1qpkmsauwln4hv7j2ygpky3fmmw84pn624yv9aruefetjals95yaattr99dteap4y7rqjr80ujvuguegc72rqjfs25mfsrfrjts+2000000+"20 a912171ffbe2d7b06e464905fd79320c3a1bab8529900ec29caaed8e.6d697072696d6572636f6e747261746f" \
--change-address addr_test1vzqtxn0jzchfcj3cee3ny250jq7fg4dqh67kfspv78ejy2scj24vv \
--mint="20 a912171ffbe2d7b06e464905fd79320c3a1bab8529900ec29caaed8e.6d697072696d6572636f6e747261746f" \
--mint-script-file ../starting/src/Minting/Plutus/Free.plutus \
--mint-redeemer-file ./work/redeemer.json \
--tx-in-collateral ba8362e55ffc29eb80cf6cee1f6cb49577b48376ec3150fe85921cd5f30885f7#3 \
--protocol-params-file ./work/pparms.json \
--out-file ./work/tx.draft

#Mint
# Step 5: Build and submit the transaction
$CARDANO_CLI transaction build \
  --babbage-era \
  --cardano-mode \
  $network \
  --tx-in "$mint_utxo_in" \
  --mint-script-file "$mint_script" \
  --mint-redeemer-file "$WORK/redeemer.json" \
  --tx-in-collateral "$COLLATERAL" \
  --tx-out "$buy_validator_script_addr+$new_total_value + 1 $thread_token_mph.$buy_token_name" \
  --change-address "$player_addr" \
  --mint "1 $ticket_mph.$ticket_num_hex" \
  --protocol-params-file "$WORK/pparms.json" \
  --out-file $WORK/buy-tx-alonzo.body

# cardano-cli transaction sign \
# --signing-key-file $SENDERKEY \
# --testnet-magic 1097911063 \
# --tx-body-file check-amount.raw \
# --out-file check-amount.signed

# cardano-cli transaction submit \
# --tx-file check-amount.signed \
# --testnet-magic 1097911063



# redeemer_file_path="$BASE/scripts/cardano-cli/$ENV/data/redeemer-buy-ticket.json"
# redeemer_mint_file_path="$BASE/scripts/cardano-cli/$ENV/data/redeemer-ticket-mint.json"
# buy_validator_addr=$(cat $BASE/scripts/cardano-cli/$ENV/data/buy-val-addr.json)
# buy_token_name=$(cat $BASE/scripts/cardano-cli/$ENV/data/buy-token-name.json | jq -r '.bytes')
# buy_token_value=$(cat $BASE/scripts/cardano-cli/$ENV/data/buy-token-value.json)
# ticket_mph=$(cat $BASE/scripts/cardano-cli/$ENV/data/ticket-policy.hash | jq -r '.bytes')
# thread_token_mph=$(cat $BASE/scripts/cardano-cli/$ENV/data/lotto-thread-token-policy.hash | jq -r '.bytes')
# player_pkh=$(cat $PLAYER_PKH)

# echo "starting lotto buy"

# # Step 1: Get UTXOs from player 
# player_addr=$($CARDANO_CLI address build $network --payment-verification-key-file $PLAYER_VKEY)
# $CARDANO_CLI query utxo --address $player_addr $network --out-file $WORK/player-utxo.json
# cat $WORK/player-utxo.json | jq -r 'to_entries[] | select(.value.value.lovelace > '$COLLATERAL_ADA' ) | .key' > $WORK/player-utxo-valid.json
# readarray player_utxo_valid_array < $WORK/player-utxo-valid.json
# player_utxo_in=$(echo $player_utxo_valid_array | tr -d '\n')

# # Step 2: Get the UTXOs from the script address
# $CARDANO_CLI query utxo --address $buy_validator_script_addr $network --out-file $WORK/buy-validator-utxo.json

# # Pull the utxo with the buy token in it
# buy_validator_utxo_tx_in=$(jq -r 'to_entries[] 
# | select(.value.value."'$thread_token_mph'"."'$buy_token_name'") 
# | .key' $WORK/buy-validator-utxo.json)


# # Step 3: Get the current datums from the utxo at the script addresses
# # TODO - filter for only utxo with thread token
# if [ "$ENV" == "devnet" ];
# then
#     cp $WORK/lotto-datum-out.json $WORK/lotto-datum-in.json
#     cp $WORK/buy-datum-out.json $WORK/buy-datum-in.json
# elif [ "$ENV" == "testnet" ]; 
# then
#     curl -H "project_id: $PROJECT_ID" "https://cardano-testnet.blockfrost.io/api/v0/addresses/$lotto_validator_script_addr/utxos" > $WORK/lotto-utxo-in.json
#     datum_hash=$(jq -r '.[0].data_hash' $WORK/lotto-utxo-in.json)
#     curl -H "project_id: $PROJECT_ID" \
#     "https://cardano-testnet.blockfrost.io/api/v0/scripts/datum/$datum_hash" | jq -c .json_value > $WORK/lotto-datum-in.json

#     curl -H "project_id: $PROJECT_ID" "https://cardano-testnet.blockfrost.io/api/v0/addresses/$buy_validator_script_addr/utxos" > $WORK/buy-utxo-in.json
#     buy_datum_hash=$(jq -r '.[0].data_hash' $WORK/buy-utxo-in.json)
#     curl -H "project_id: $PROJECT_ID" \
#     "https://cardano-testnet.blockfrost.io/api/v0/scripts/datum/$buy_datum_hash" | jq -c .json_value > $WORK/buy-datum-in.json
    
#     # Check if this is the first buy, if so, then need to copy buy datum from prior output
#     # This is needed because the startbuy only sends a datum hash and not the actual datum
#     buy_datum_test=$(cat $WORK/buy-datum-in.json)
#     if [ "$buy_datum_test" == "null" ];
#     then 
#         cp $BASE/scripts/cardano-cli/$ENV/data/buy-datum-startbuy.datum $WORK/buy-datum-in.json
#     fi
# elif [ "$ENV" == "mainnet" ];
# then
#     curl -H "project_id: $PROJECT_ID" "https://cardano-mainnet.blockfrost.io/api/v0/addresses/$lotto_validator_script_addr/utxos" > $WORK/lotto-utxo-in.json
#     datum_hash=$(jq -r '.[0].data_hash' $WORK/lotto-utxo-in.json)
#     curl -H "project_id: $PROJECT_ID" \
#     "https://cardano-mainnet.blockfrost.io/api/v0/scripts/datum/$datum_hash" | jq -c .json_value > $WORK/lotto-datum-in.json

#     curl -H "project_id: $PROJECT_ID" "https://cardano-mainnet.blockfrost.io/api/v0/addresses/$buy_validator_script_addr/utxos" > $WORK/buy-utxo-in.json
#     buy_datum_hash=$(jq -r '.[0].data_hash' $WORK/buy-utxo-in.json)
#     curl -H "project_id: $PROJECT_ID" \
#     "https://cardano-mainnet.blockfrost.io/api/v0/scripts/datum/$buy_datum_hash" | jq -c .json_value > $WORK/buy-datum-in.json
    
#     # Check if this is the first buy, if so, then need to copy buy datum from prior output
#     # This is needed because the startbuy only sends a datum hash and not the actual datum
#     buy_datum_test=$(cat $WORK/buy-datum-in.json)
#     if [ "$buy_datum_test" == "null" ];
#     then 
#         cp $BASE/scripts/cardano-cli/$ENV/data/buy-datum-startbuy.datum $WORK/buy-datum-in.json
#     fi
# else
#     echo "No environment selected"
#     exit 1
# fi

# # Step 4: Get values from the lotto and buy script datums  
# ticket_cost=$(jq -r '.fields[0].fields[8].int' $WORK/lotto-datum-in.json)
# seq_num=$(jq -r '.fields[3].int' $WORK/lotto-datum-in.json)
# ticket_num="$seq_num$2"
# ticket_num_hex=$(echo -n "$ticket_num" | xxd -ps)
# ticket_total=$(jq -r '.fields[0].int' $WORK/buy-datum-in.json)
# total_value=$(jq -r '.fields[1].int' $WORK/buy-datum-in.json)
# new_ticket_total=$(($ticket_total + 1))
# new_total_value=$(($total_value + ($ticket_cost * 100)))


# # Upate the buy datum accordingly
# cat $WORK/buy-datum-in.json | \
# jq -c '
#   .fields[0].int   |= '$new_ticket_total'
# | .fields[1].int   |= '$new_total_value'' > $WORK/buy-datum-out.json
 
# # Upate the redeemer mint with the ticket number to be purchased
# cat $redeemer_mint_file_path | \
# jq -c '
#   .fields[0].constructor    |= '1'
# | .fields[1].bytes          |= "'$ticket_num_hex'"' > $WORK/redeemer-ticket-mint.json



# # Step 5: Build and submit the transaction
# $CARDANO_CLI transaction build \
#   --alonzo-era \
#   --cardano-mode \
#   $network \
#   --tx-in "$player_utxo_in" \
#   --tx-in "$buy_validator_utxo_tx_in" \
#   --tx-in-script-file "$buy_validator_script" \
#   --tx-in-datum-file "$WORK/buy-datum-in.json" \
#   --tx-in-redeemer-file "$redeemer_file_path" \
#   --mint-script-file "$minting_script" \
#   --mint-redeemer-file "$WORK/redeemer-ticket-mint.json" \
#   --tx-in-collateral "$PLAYER_COLLATERAL" \
#   --tx-out "$buy_validator_script_addr+$new_total_value + 1 $thread_token_mph.$buy_token_name" \
#   --tx-out-datum-embed-file "$WORK/buy-datum-out.json" \
#   --tx-out "$player_addr+$MIN_ADA_OUTPUT_TX + 1 $ticket_mph.$ticket_num_hex" \
#   --change-address "$player_addr" \
#   --mint "1 $ticket_mph.$ticket_num_hex" \
#   --protocol-params-file "$WORK/pparms.json" \
#   --out-file $WORK/buy-tx-alonzo.body
  

# # --calculate-plutus-script-cost "$BASE/scripts/cardano-cli/$ENV/data/buy-alonzo.costs"

# echo "tx has been built"

# $CARDANO_CLI transaction sign \
#   --tx-body-file $WORK/buy-tx-alonzo.body \
#   $network \
#   --signing-key-file "${PLAYER_SKEY}" \
#   --out-file $WORK/buy-tx-alonzo.tx

# echo "tx has been signed"

# echo "Submit the tx with plutus script and wait 5 seconds..."
# $CARDANO_CLI transaction submit --tx-file $WORK/buy-tx-alonzo.tx $network



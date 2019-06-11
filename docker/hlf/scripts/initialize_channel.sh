set -ev

export FABRIC_CHANNEL_TIMEOUT=3

## Create the channel
peer channel create -o ${CORE_ORDERER_ADDRESS} -c mychannel -f /etc/hyperledger/configtx/channel.tx
sleep ${FABRIC_CHANNEL_TIMEOUT}
peer channel join -b mychannel.block
sleep 10


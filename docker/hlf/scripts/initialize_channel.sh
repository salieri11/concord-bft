#  this sets up a channel between orderer and peer, which enables peer chaincode 
#
# assuming HLF_HARDCODED_CONFIG

set -ev

export FABRIC_CHANNEL_TIMEOUT=3
## need orderer and peer to be running before attempting to create a channel between them 
## agent will just start orderer, peer, and tools docker containers on vms  
## a 10 second sleep is quick simple solution 
sleep 10

## Create the channel
peer channel create -o ${CORE_ORDERER_ADDRESS} -c mychannel -f /etc/hyperledger/configtx/channel.tx
sleep ${FABRIC_CHANNEL_TIMEOUT}
peer channel join -b mychannel.block
sleep 10


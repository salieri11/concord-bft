#!/bin/bash

#host=10.193.11.107
host=$1

helen_user=$2

ssh ${helen_user}@${host} 'docker stop $(docker ps -aq) && docker rm $(docker ps -aq)' 2> /dev/null
ssh -t ${helen_user}@${host} "sudo rm -r -f /home/${helen_user}/helen"
ssh ${helen_user}@${host} "mkdir -p /home/${helen_user}/helen"
scp "application.properties" ${helen_user}@${host}:/home/${helen_user}/helen/
scp "docker-compose-helen.yml" ${helen_user}@${host}:/home/${helen_user}/helen/docker-compose.yml
ssh ${helen_user}@${host} "cd /home/${helen_user}/helen && docker-compose up" > ./helen_${host}_docker.log &

echo ""
sleep 15
echo "docker-compose up on helen node... done!"


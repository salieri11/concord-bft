#!/bin/sh
  
echo "Stop & removing all containers except agent, and then restarting agent"
all_image_ids_except_agent=`docker images | grep -v '/agent' | tr -s " " | cut -d ' ' -f3 | grep -v IMAGE`
echo $all_image_ids_except_agent
for image_id in `echo $all_image_ids_except_agent`
do
    echo $image_id
    container_to_remove=`docker ps -a | grep $image_id | tr -s " " | cut -d ' ' -f1`
    docker stop $container_to_remove
    docker rm -f $container_to_remove
done
sleep 5
docker restart agent
sleep 5


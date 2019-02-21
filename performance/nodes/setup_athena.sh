#!/bin/bash

rm -rf config-athena*

config_name="config-athena"

log4jcplus="log4cplus.properties"
athena_config="athena.config"


dir="s16_reps/"
client_name="s_f5c0_cli"
replica_name="s_f5c0_rep"
pub_config="config-public/s_f5c0_config.pub"

# number of nodes
N=$1

# number of clients of each replica
M=N

if [ $N -eq 4 ]; then
    dir="s4_reps/"
    client_name="s_f1c0_cli"
    replica_name="s_f1c0_rep"
    rm -f ${pub_config}
    pub_config="config-public/s_f1c0_config.pub"
else
    rm -f config-public/s_f1c0_config.pub
fi


i=1
while([ $i -le $N ])
do
    if [ -e ${config_name}${i} ]
    then
        echo "${config_name}${i} exits"
        rm -r ${config_name}${i}
    fi
    mkdir -p ${config_name}${i}
    let "i++"
done


i=0
while([ $i -lt $N ])
do
    # copy the pub file into each folder
    cp ${pub_config} "${config_name}$(( i+1 ))/"

    # copy the athena config file into each folder
    cp ${athena_config} "${config_name}$(( i+1 ))/"

    # copy the replica key into each folder
    suffix=""
    if [ $i -lt 9 ]
    then
        suffix="0"$(( i+1 ))
        #echo "${suffix} less 9"
    else
        suffix=$(( i+1 ))
    fi
    cp "${dir}${replica_name}${suffix}" "${config_name}$(( i+1 ))/${replica_name}${suffix}.priv"
    if [ $N -eq 4 ]; then
        echo "public=/athena/config-local/s_f1c0_config.pub" >> "${config_name}$(( i+1 ))/${athena_config}"
    else
        echo "public=/athena/config-local/s_f5c0_config.pub" >> "${config_name}$(( i+1 ))/${athena_config}"
    fi

    echo "replica=/athena/config-local/${replica_name}${suffix}.priv" >> "${config_name}$(( i+1 ))/${athena_config}"

    # copy the clients keys into each folder
    j=1
    while(( $j<=M ))
    do
        index=$(( i*M+j ))
        #echo "${index}"
        if [ $index -le 9 ]; then
            suffix="0"${index}
        else
            suffix=${index}
        fi
        cp "${dir}${client_name}${suffix}" "${config_name}$(( i+1 ))/${client_name}${suffix}.priv"
        echo "client=/athena/config-local/${client_name}${suffix}.priv" >> "${config_name}$(( i+1 ))/${athena_config}"
        let "j++"
    done

    # copy log4cpus into each folder
    cp ${log4jcplus} "${config_name}$(( i+1 ))/"

    let "i++"
done

declare -a hosts
let i=0
while IFS='' read -r host || [[ -n "$host" ]]; do
     hosts[i]=$host
     ((++i))
done < "$2"


concord_user=$3

i=1
for host in ${hosts[@]}; do
    echo "%%%%%%%%%%%%%%%%%%%% $host %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%"
    ssh "${concord_user}@${host}" 'docker stop $(docker ps -aq) && docker rm $(docker ps -aq)' 2> /dev/null
    
    ssh -t ${concord_user}@${host} 'sudo rm -r -f' /home/${concord_user}/athena

    ssh ${concord_user}@${host} 'mkdir -p' "/home/${concord_user}/athena/config-public"
    scp config-public/* ${concord_user}@${host}:/home/${concord_user}/athena/config-public

    ssh ${concord_user}@${host} 'mkdir -p' /home/${concord_user}/athena/config-athena
    scp ${config_name}${i}/* ${concord_user}@${host}:/home/${concord_user}/athena/config-athena

    ssh ${concord_user}@${host} 'mkdir -p' /home/${concord_user}/athena/log /home/${concord_user}/athena/rocksdbdata
    scp "docker-compose.yml" ${concord_user}@${host}:/home/${concord_user}/athena
    ((++i))
done


for host in ${hosts[@]}; do
    ssh ${concord_user}@${host} "cd /home/${concord_user}/athena && docker-compose up" > ./athena_${host}_docker.log & 
done

echo ""
sleep 5
echo "docker-compose up on all athena nodes... done!"


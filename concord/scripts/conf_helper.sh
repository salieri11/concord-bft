#!/bin/bash


if  [[ $1 = "-h" ]] || [ -z $1 ]; then
	echo "----Concord conf helper----------"
	echo ""
	echo "[-c] copies application input from the given replica into a given relative path"
	echo "example: CONF_PATH=. ./conf_helper.sh -p password -c 10.70.30.34"
	echo ""
	echo "[-a] distruibutes application configuration from the given path to the given replicas"
	echo "example: CONF_PATH=./ ./conf_helper.sh -p password -a 10.70.30.34 10.73.232.21"
	echo ""
	echo "[-s] distruibutes secrets conf file from the given path to the given replicas"
	echo "example: CONF_PATH=. ./conf_helper.sh -p password -s 10.70.30.34 10.73.232.21"
	echo ""
	echo "[-d] distruibutes deployment conf file from the given path to the given replicas"
	echo "example: CONF_PATH=. ./conf_helper.sh -p password -d 10.70.30.34 10.73.232.21"
	echo ""
	exit
fi



#####options that require password##########


if  [[ $1 != "-p" ]]; then
	echo "Please supply Concord password with -p"
	exit
fi

shift
PASS=$1
shift

if  [[ $1 = "-c" ]]; then
	echo "copy from $2"
	sshpass -p $PASS ssh -o StrictHostKeyChecking=no root@$2 bash -s <<'EOF'
        	rm -rf /tmp/app_conf 2>/dev/null
        	mkdir -p /tmp/app_conf
        	docker cp -a concord:/concord/resources/appconf_input.yaml /tmp/app_conf
			cp -f /config/concord/config-public/appconf_input.yaml /tmp/app_conf
        	exit
EOF

	mkdir -p ./$CONF_PATH
	sshpass -p $PASS scp -r root@$2:/tmp/app_conf/* ./$CONF_PATH
	echo "copied to $CONF_PATH/appconf_input.yaml"
	exit
fi


if  [[ $1 = "-a" ]]; then
	CONF_FILE="$CONF_PATH/appconf_input.yaml"
	echo "looking for $CONF_FILE"
	if [ ! -f $CONF_FILE ]; then
    		echo "can't find $CONF_FILE"
		exit
	fi
	shift
	for var in "$@"
	do
	echo "copy $CONF_FILE to $var"
	sshpass -p $PASS scp -o StrictHostKeyChecking=no -r $CONF_FILE root@$var:/tmp
	sshpass -p $PASS ssh -o StrictHostKeyChecking=no root@$var bash -s <<'EOF'
	DATE=$(date +%s)
	cp /config/concord/config-local/appconf_input.yaml /config/concord/config-local/appconf_input.yaml_$DATE 2>/dev/null
  	cp /tmp/appconf_input.yaml /config/concord/config-local
	chmod 666 /config/concord/config-local/appconf_input.yaml
  	exit
EOF
	done
	exit
fi


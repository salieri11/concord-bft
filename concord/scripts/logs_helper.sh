#!/bin/bash

declare -a LEVELS=("TRACE" "DEBUG" "INFO " "WARN " "ERROR" "FATAL" "OFF")

if  [[ $1 = "-h" ]] || [ -z $1 ]; then
	echo "----Concord logs helper----------"
	echo ""
	echo "[-c] copies log4cplus.properties from the given replica into a given relative path"
	echo "example: PRP_PATH=. ./logs_helper.sh -p password -c 10.70.30.34"
	echo ""
	echo "[-m] distruibutes log4cplus.properties from the given path to the given replicas"
	echo "example: PRP_PATH=./log4prop ./logs_helper.sh -p password -m 10.70.30.34 10.73.232.21"
	echo ""
	echo "[-f] fetches application logs from replicas and prticipant (if supplied)"
	echo "example: PARTICIPANT=10.40.205.63 ./logs_helper.sh -p password -f 10.40.205.56 10.40.205.57"
	echo ""
	echo "[-s] Set logger level in file"
	echo "example: PRP_PATH=. ./logs_helper.sh -s concord.bft.consensus DEBUG"
	echo ""
	echo "[-a] Add new logger to file"
	echo "params: \${logger_name} \${level}"
	echo "logger_name - full qualified name e.g:"
	echo "* concord.bft.consensus"
	echo "* com.vmware.concord.KVBClientPool"
	echo "* concord.tls"
	echo "level: \"TRACE\" \"DEBUG\" \"INFO\" \"WARN\" \"ERROR\" \"FATAL\" \"OFF\""
	echo "example: PRP_PATH=. ./logs_helper.sh -a concord.bft.consensus DEBUG"
	echo ""
	echo "[-d] Deletes a logger from file"
	echo "params: \${logger_name}"
	echo "logger_name - full qualified name e.g:"
	echo "* concord.bft.consensus"
	echo "* com.vmware.concord.KVBClientPool"
	echo "* concord.tls"
	echo "example: PRP_PATH=. ./logs_helper.sh -d concord.bft.newlogger"
	exit
fi


if  [[ $1 = "-s" ]] || [[ $1 = "-d" ]]; then
	PROP_FILE="$PRP_PATH/log4cplus.properties"
	if [ ! -f "$PRP_PATH/log4cplus.properties" ]; then
    		echo "can't find log4cplus.properties at $PRP_PATH"
		exit
	fi
	NEW_PROP_FILE=$PROP_FILE
	NEW_PROP_FILE+="_tmp"
	LOGGER=$2
	LEVEL=$3
	if [[ ! " ${LEVELS[@]} " =~ " ${LEVEL} " ]]; then
    	echo "$LEVEL not supported"
		exit
	fi
	LINE=$(cat $PROP_FILE |grep  "$LOGGER=" |grep -i "logger")
	if [ -z "$LINE" ]
	then
		echo "No $LOGGER is currently defined, use -a to add."
		exit
	else
		COUNT=$(echo "$LINE" | wc -l)
		if [ $COUNT -gt 1 ]; then
			echo "Ambigious $LOGGER, found $COUNT -- [$LINE]"
			exit
		fi
		
		# if [ $1 = "-d" ]
		# then
		# 	sed  "/$LOGGER/d" $PROP_FILE > "$NEW_PROP_FILE"
		# fi
	fi
	if [ $1 = "-d" ]
	then
		echo "Delete $LOGGER"
		sed  "/$LOGGER/d" $PROP_FILE > "$NEW_PROP_FILE"
	fi
	if [ $1 = "-s" ]
	then
		echo "Set $LOGGER to $LEVEL"
		sed  "s/$LOGGER=[A-Z]*/$LOGGER=$LEVEL/g" $PROP_FILE > "$NEW_PROP_FILE"
	fi
	DATE=$(date +%s)
	BACK=$PROP_FILE
	BACK+=$DATE
	mv $PROP_FILE $BACK
	mv $NEW_PROP_FILE $PROP_FILE
	echo "##############################$PROP_FILE##############################"
	cat $PROP_FILE
	exit
fi

if  [[ $1 = "-a" ]]; then
	PROP_FILE="$PRP_PATH/log4cplus.properties"
	if [ ! -f "$PRP_PATH/log4cplus.properties" ]; then
    		echo "can't find log4cplus.properties at $PRP_PATH"
		exit
	fi
	shift
	NEW_PROP_FILE=$PROP_FILE
	NEW_PROP_FILE+="_tmp"
	LOGGER=$1
	LEVEL=$2
	if [ -z "$LEVEL" ]
	then
    	echo "please provide logger level"
		exit
	fi
	if [[ ! " ${LEVELS[@]} " =~ " ${LEVEL} " ]]; then
    	echo "$LEVEL not supported"
		exit
	fi
	LINE=$(cat $PROP_FILE |grep $LOGGER)
	if [ -z "$LINE" ]
	then
		echo "Adding log4cplus.logger.$LOGGER=$LEVEL"
		cp $PROP_FILE $NEW_PROP_FILE
		echo "" >> "$NEW_PROP_FILE"
		echo "log4cplus.logger.$LOGGER=$LEVEL" >> "$NEW_PROP_FILE"
	else
		echo "$LOGGER already exists, to set its level use -s"
		exit
	fi
	DATE=$(date +%s)
	BACK=$PROP_FILE
	BACK+=$DATE
	mv $PROP_FILE $BACK
	mv $NEW_PROP_FILE $PROP_FILE
	echo "##############################$PROP_FILE##############################"
	cat $PROP_FILE
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
        	rm -rf /tmp/log4prop 2>/dev/null
        	mkdir -p /tmp/log4prop
        	docker cp -a concord:/concord/resources/log4cplus.properties /tmp/log4prop
        	exit
EOF

	mkdir -p ./$PRP_PATH
	sshpass -p $PASS scp -r root@$2:/tmp/log4prop/* ./$PRP_PATH
	echo "copied to $PRP_PATH/log4cplus.properties"
	exit
fi


if  [[ $1 = "-m" ]]; then
	PROP_FILE="$PRP_PATH/log4cplus.properties"
	echo "looking for $PROP_FILE"
	if [ ! -f "$PRP_PATH/log4cplus.properties" ]; then
    		echo "can't find log4cplus.properties at $PRP_PATH"
		exit
	fi
	shift
	for var in "$@"
	do
	echo "copy $PROP_FILE to  $var"
	sshpass -p $PASS scp -o StrictHostKeyChecking=no -r $PROP_FILE root@$var:/tmp
	sshpass -p $PASS ssh -o StrictHostKeyChecking=no root@$var bash -s <<'EOF'
	chmod 666 /tmp/log4cplus.properties
	DATE=$(date +%s)
	docker cp concord:/concord/resources/log4cplus.properties /tmp/log4cplus.properties_$DATE
	docker cp /tmp/log4cplus.properties_$DATE concord:/concord/resources
  	docker cp /tmp/log4cplus.properties concord:/concord/resources
  	exit
EOF
	done
	exit
fi

if  [[ $1 = "-f" ]]; then
	if [[ ! -z $PARTICIPANT ]]; then
		echo "Collecting from participant $PARTICIPANT "
		sshpass -p $PASS ssh -o StrictHostKeyChecking=no root@$PARTICIPANT bash -s <<'EOF'
			rm -r /tmp/damlog
			mkdir -p /tmp/damlog
			docker cp -a daml_ledger_api:/doc/daml/daml-ledger-api.log /tmp/damlog
			ls /tmp/damlog
			echo finsihed...
			exit
EOF
		mkdir -p $PARTICIPANT
		sshpass -p $PASS scp -r  -o GlobalKnownHostsFile=/dev/null -o UserKnownHostsFile=/dev/null root@$PARTICIPANT:/tmp/damlog/* ./$PARTICIPANT

	fi
	shift
	for var in "$@"
	do
	echo "copy from $var..."
	sshpass -p $PASS ssh -o StrictHostKeyChecking=no root@$var bash -s <<'EOF'
			tdnf install -y tar	
			cd /var/lib/docker/containers
			rm -f docker_logs.tar.gz
			tar -czvf docker_logs.tar.gz ./*
			cd /config/concord
			rm -f ./rocksdbdata.tar.gz
			tar -czvf rocksdbdata.tar.gz ./rocksdbdata
			exit
EOF

	mkdir -p ./$var
	echo "before scp"
	sshpass -p $PASS scp -r root@$var:/var/lib/docker/containers/docker_logs.tar.gz ./$var
	sshpass -p $PASS scp -r root@$var:/config/concord/rocksdbdata.tar.gz ./$var
	echo "done"
	done
	exit
fi


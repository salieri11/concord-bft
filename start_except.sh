#!/bin/bash
# This script runs all the containers except for the one specified
# Valid arguments are concord, db, helen, ui, aux
# For example, if you want to bring up all of the system except for helen use
# ./start_except.sh helen
dont_start=${1}
if [[  ! "$dont_start" =~ ^(concord|db|helen|ui|aux)$ ]]; then
    echo "Usage: one of concord, db, helen, ui, aux"
    exit 1
fi
if [ $dont_start != "concord" ]; then
	echo "Starting concord"
	docker-compose -f docker-compose-concord.yml up -d
fi

if [ $dont_start != "db" ]; then
	echo "Starting db"
	docker-compose -f docker-compose-db.yml up -d
fi

if [ $dont_start != "helen" ]; then
	echo "Starting helen"
	docker-compose -f docker-compose-helen.yml up -d
fi

if [ $dont_start != "ui" ]; then
	echo "Starting ui"
	docker-compose -f docker-compose-ui.yml up -d
fi

if [ $dont_start != "aux" ]; then
	echo "Starting aux"
	docker-compose -f docker-compose-aux.yml up -d
fi

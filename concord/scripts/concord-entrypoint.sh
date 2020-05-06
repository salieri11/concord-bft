#!/bin/bash

set -e

DATE_CMD="date --iso-8601=seconds | awk -F'+' '{print \$1}'"
echo $(eval $DATE_CMD) "$0 $@"

if [ "$#" -eq 0  ]; then
	sysctl kernel.core_pattern=/concord/cores/core.%e.%h.%s.%t >/dev/null
	ulimit -c unlimited
	exec /concord/concord -c /concord/config-local/concord.config
elif [ "$1" = 'debug' ]; then
	exec gdb
else
	exec "$@"	
fi

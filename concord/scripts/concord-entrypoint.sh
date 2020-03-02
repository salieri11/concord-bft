#!/bin/bash

echo ==========  RUNNING CONCORD ==========
echo "$0 $@"
echo ======================================

if [ "$#" -eq 0  ]; then
	sysctl kernel.core_pattern=/concord/cores/core.%e.%h.%s.%t
	ulimit -c unlimited
	/concord/concord -c /concord/config-local/concord.config
elif [ "$1" = 'debug' ]; then
	exec gdb
else
	exec "$@"	
fi

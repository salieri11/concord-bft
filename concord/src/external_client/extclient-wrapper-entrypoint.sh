#!/bin/bash

sysctl kernel.core_pattern=/opt/cores/core.%e.%h.%s.%t >/dev/null
ulimit -c unlimited

cp /opt/external_client_wrp /opt/cores/

if [ $START_GDBSERVER = "1" ]
then
    exec gdbserver 0.0.0.0:5678 /opt/external_client_wrp /concord/config-public/bftclient.config
else
    exec /opt/external_client_wrp /concord/config-public/bftclient.config
fi
#!/bin/bash
/cockroach/cockroach start --log-dir /var/tmp/ --insecure --http-port 8081 --background
/cockroach/cockroach user set helen_admin --insecure
/cockroach/cockroach --host localhost sql --insecure < /opt/vmbc/helen/schema.sql
tail -f /dev/null
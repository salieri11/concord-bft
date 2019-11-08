#!/bin/bash
/cockroach/cockroach start --log-dir /var/tmp/ --insecure --http-port 8081 --background --cache 25% --max-sql-memory 25%
/cockroach/cockroach user set helen_admin --insecure
/cockroach/cockroach --host localhost sql --insecure < /opt/vmbc/helen/schema.sql
tail -f /dev/null

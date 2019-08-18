#!/bin/bash
/cockroach/cockroach start --store=path=/opt/cockroach/data/ --log-dir /var/tmp/ --insecure --http-port 8081 --host helen-db.us-west-2.vdp-stg.vmware.com --background --cache 25% --max-sql-memory 25%
tail -f /dev/null
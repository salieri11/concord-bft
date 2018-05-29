#!/bin/bash

# Start the cockroach db first node
# This commands needs to be run manually first
# cockroach start \
#           --insecure \
#           --host=localhost \
#           --http-port=8081 \
#           --store=/tmp/helenDB

# Start few more nodes with following command to make a cluster
# Note that for every node the `store` parameter, `port` parameter and
# `http-port` parameter must have different value. Also, join parameter
# is important.

# cockroach start \
#           --insecure \
#           --store=/tmp/helenDB2 \
#           --host=localhost \
#           --port=26258 \
#           --http-port=8082 \
#           --join=localhost:26257

# Create a cockroach db user, Following command creates a new cockroach db
# user named `helen_admin`
cockroach user set helen_admin --insecure

# Now create a helen database
cockroach sql --insecure -e 'CREATE DATABASE helen'

# Allow helen_admin all access to helen database
cockroach sql --insecure -e 'GRANT ALL ON DATABASE helen TO helen_admin'


# We can also connect to that cluster with following command

# cockroach sql --insecure

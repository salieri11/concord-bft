## Cockroach DB Setup

Run following commands to setup cockroach DB

## First install cockroach DB by following instructions on their website.

## Start the cockroach db first node

```
cockroach start \
          --insecure \
          --host=localhost \
          --http-port=8081 \
          --store=/tmp/helenDB
```

Start few more nodes with following command to make a cluster.
However, This is optional (we can run cockroach db cluster with only
1 node)

```
cockroach start \
          --insecure \
          --store=/tmp/helenDB2 \
          --host=localhost \
          --port=26258 \
          --http-port=8082 \
          --join=localhost:26257
```

Note that for every node the `store` parameter, `port` parameter and
`http-port` parameter must have different values. Also, join parameter
is important.


## Create a cockroach db user

Following command creates a new cockroach db  user named `helen_admin`

```
cockroach user set helen_admin --insecure
```


## Run database setup script
```
cockroach sql --insecure < src/main/resources/database/schema.sql
```


# Verify if setup is properly done
Connect to that cluster with following command and run normal SQL queries to
verify things.

```
cockroach sql --insecure
```

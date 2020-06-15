# format-code
Format concord code using clang format

usage:
```./format-code.sh ${concord_dir}```

# concord entry point

The script that docker runs in order to start concord replica.

usage:
* ```docker run -it concord debug``` - to start the container in gdb mode.

# logs_helper

# PreRequisite

## sshpass 
https://gist.github.com/arunoda/7790979
on mac homebrew doesn't work (last time I've tried), need to perform `configuire and make`
as listed in the link


## Distruibute log4cplus.properties from the given path to the given committer nodes

Option: `-m`

Params: 
* env: PRP_PATH - a path which contains the log4cplus.properties file.
* cmd arg: list of committer nodes IPs.
* Example: ```PRP_PATH=. ./logs_helper.sh -p password -m 10.70.30.34 10.73.232.21```

## Fetch application logs from committer nodes and participant (if supplied)

Option: `-f`

Params: 
* env: PARTICIPANT - IP of the participant node.
* cmd arg: list of committer nodes IPs.
* Example: ```PARTICIPANT=10.40.205.63 ./logs_helper.sh -p password -f 10.40.205.56 10.40.205.57```

## Copy log4cplus.properties from a given committer node

Option: `-c`

Param: 
* cmd arg: Ip of a committer node
* Example:```./logs_helper.sh -p password -c 10.70.30.34```

## Set logger level in file

**This command changes the logger level, The desired logger must exist in the properties file.**

Option: `-s`

Params: 
* env: PRP_PATH - a path which contains the log4cplus.properties file.
* cmd arg: 
    * logger name
    * level {"FATAL" ERROR" "WARN" "INFO" "DEBUG" "TRACE" "OFF"} (case sensitive)
* Example: ```PRP_PATH=. ./logs_helper.sh -s concord.bft.consensus DEBUG```

The properties file in PRP_PATH will be overriden, the orig file will be saved with time postfix

## Add logger to file

Provide a full qualified name of the logger e.g. concord.bft.consensus
The script doesn't verify that the provided logger exists in Concord. it will be added to the file.
In case it doesn't exists, Concord logger implementation will ignore it and continue as normal.

Option: `-a`

Params: 
* env: PRP_PATH - a path which contains the log4cplus.properties file.
* cmd arg: 
    * Full qualified logger name
    * level {"ERROR" "WARN" "INFO" "DEBUG" "TRACE"} (case sensitive)
* Example: ```PRP_PATH=. ./logs_helper.sh -s concord.bft.consensus DEBUG```

## Add logger to file

Provide a full qualified name of the logger e.g. concord.bft.consensus
The script doesn't verify that the provided logger exists in Concord. it will be added to the file.
In case it doesn't exists, Concord logger implementation will ignore it and continue as normal.

Option: `-a`

Params: 
* env: PRP_PATH - a path which contains the log4cplus.properties file.
* cmd arg: 
    * Full qualified logger name
    * level {"ERROR" "WARN" "INFO" "DEBUG" "TRACE"} (case sensitive)
* Example: ```PRP_PATH=. ./logs_helper.sh -s concord.bft.consensus DEBUG```


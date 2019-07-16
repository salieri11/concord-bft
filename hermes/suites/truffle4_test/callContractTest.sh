#!/bin/bash
# We'll have to test multiple versions of Truffle
# eventually, so just do local "installations".
cp -r /var/jenkins/workspace/truffle4_resources/node_modules .

for i in `find . -name "*.json"`
do
    sed -i 's?/var/jenkins/workspace/truffle4_resources?`pwd`?g' "${i}"
done

# Credentials are stored in credentials.txt and put into truffle.js at run time.
# This is done so that we can put placeholders in credentials.txt and have CI/CD
# overwrite them (later).
# Having them in a file called "credentials.txt" makes it easier for people who
# are running the tests on their local systems; they don't have to fish for where
# the credentials go.
read -d '' -a credentials < "credentials.txt"

USR=${credentials[0]}
KEY=${credentials[1]}
cp truffle.js truffle.js.bkp
sed "s/USER_PLACEHOLDER/$USR/g" truffle.js > truffle_tmp.js
sed "s/KEY_PLACEHOLDER/$KEY/g" truffle_tmp.js > truffle.js

EXEC=./node_modules/.bin/truffle
echo Truffle: ${EXEC}

"${EXEC}" compile
"${EXEC}" migrate --network development --reset
"${EXEC}" exec --network development hello.js > out.log

rm -rf node_modules

RESULT=`cat out.log`
echo $RESULT | grep "Hello, World!"
EXIT_CODE=$?

if [ ${EXIT_CODE} -eq 0 ]
then
    echo Test passed
    exit 0
else
    echo Truffle exec output: \""${RESULT}"\"
    exit 1
fi

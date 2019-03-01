#!/bin/bash
# We'll have to test multiple versions of Truffle
# eventually, so just do local "installations".
cp -r /var/jenkins/workspace/truffle4_resources/node_modules .

for i in `find . -name "*.json"`
do
    sed -i 's?/var/jenkins/workspace/truffle4_resources?`pwd`?g' "${i}"
done

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

#!/bin/bash
echo Building ballotApp
cd ../ballotApp
mvn clean install assembly:single

echo Building quickstart
cd ../daml/quickstart/
daml build
mvn clean install

echo Building benchmark
cd ../../benchmark/
mvn clean install


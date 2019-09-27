#!/bin/bash
echo Building ballotApp
cd ../ballotApp
mvn clean install assembly:single
echo Building benchmark
cd ../benchmark
mvn clean install assembly:single

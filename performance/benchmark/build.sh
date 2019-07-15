#!/bin/bash

echo Building ballotApp and benchmark...
cd ../ballotApp
mvn clean install assembly:single
cd ../benchmark
mvn clean package

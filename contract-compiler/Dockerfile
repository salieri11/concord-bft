FROM node:8.9.4-slim as node
LABEL Description="VMware Blockchain Contract Compiler Microservice"

## Setup a new non-root user and a group ##
RUN groupadd -r ccgrp && useradd -rm -d /home/ccuser -s /bin/bash -g ccgrp ccuser

## Setup the app ##
ENV APP_HOME=/usr/src/app
WORKDIR $APP_HOME
COPY --chown=ccuser:ccgrp . $APP_HOME

## Installations ##
RUN npm install \
 && bash download_solc.sh \
 && sed -i "s@'https://ethereum.github.io/solc-bin/bin/soljson-'@'http://127.0.0.1:3000/static/solc-bin/bin/soljson-'@" /usr/src/app/node_modules/solc/wrapper.js \
 && sed -i "s@https.get(url, function (response) {@var http = require('http');\n      http.get(url, function (response) {@" /usr/src/app/node_modules/solc/wrapper.js

## Change to non-root user ##
USER ccuser
EXPOSE 3000
CMD [ "npm", "start" ]

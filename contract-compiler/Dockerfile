FROM node:8.9.4-slim as node
LABEL Description="VMware Blockchain Contract Compiler Microservice"

WORKDIR /usr/src/app

COPY .npmrc ./
COPY package*.json ./

RUN npm install

COPY . .

RUN bash download_solc.sh

RUN sed -i "s@'https://ethereum.github.io/solc-bin/bin/soljson-'@'http://127.0.0.1:3000/static/solc-bin/bin/soljson-'@" /usr/src/app/node_modules/solc/wrapper.js

RUN sed -i "s@https.get(url, function (response) {@var http = require('http');\n      http.get(url, function (response) {@" /usr/src/app/node_modules/solc/wrapper.js

EXPOSE 3000
CMD [ "npm", "start" ]

FROM node:8.9.4 as node
LABEL Description="VMware Blockchain Contract Compiler Microservice"

WORKDIR /usr/src/app

COPY .npmrc ./
COPY package*.json ./

RUN npm install

COPY . .

EXPOSE 3000
CMD [ "npm", "start" ]

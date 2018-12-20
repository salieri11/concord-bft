FROM ubuntu:16.04
LABEL maintainer="mmuraliram@vmware.com"
WORKDIR /source
COPY . /source
RUN apt-get -y update
RUN apt-get -y upgrade
RUN apt-get install -y git
RUN apt-get update -yq \
    && apt-get install curl gnupg -yq \
    && curl -sL https://deb.nodesource.com/setup_8.x | bash \
    && apt-get install nodejs -yq
RUN apt-get install -y software-properties-common
RUN echo | add-apt-repository ppa:ethereum/ethereum
RUN apt-get update -y
RUN apt-get install -y solc
RUN echo | npm install web3@0.20.1
RUN npm install --save httpheaderprovider
RUN npm install --global mocha
RUN apt-get install -y vim

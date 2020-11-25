FROM athena-docker-local.artifactory.eng.vmware.com/ubuntu:18.04

RUN echo 'deb http://artifactory.eng.vmware.com/ubuntu-remote/ bionic main restricted multiverse universe\n' > /etc/apt/sources.list
RUN echo 'deb http://artifactory.eng.vmware.com/ubuntu-remote/ bionic-updates main restricted multiverse universe\n' >> /etc/apt/sources.list
RUN echo 'deb http://artifactory.eng.vmware.com/ubuntu-remote/ bionic-security main restricted multiverse universe\n' >> /etc/apt/sources.list

RUN apt-get update && apt-get install openjdk-11-jdk -y && apt-get install maven -y

RUN groupadd --system --gid 1000 test
RUN useradd --system --gid test --uid 1000 --shell /bin/bash --create-home test
USER test
WORKDIR /home/test

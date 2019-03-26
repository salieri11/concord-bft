FROM ubuntu:18.10

RUN apt-get update && apt-get install openjdk-11-jdk -y && apt-get install maven -y

RUN groupadd --system --gid 1000 test
RUN useradd --system --gid test --uid 1000 --shell /bin/bash --create-home test
USER test
WORKDIR /home/test

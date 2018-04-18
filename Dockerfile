# Build Front End image
FROM node:8.9.4
LABEL Description="Build environment for Athena UI"

RUN npm install -g @angular/cli --unsafe

WORKDIR /helen

COPY ./webapp /helen
RUN npm install
# Rebuild node-sass to ensure we have the correct binding for our container
RUN npm rebuild node-sass
RUN npm run build

## Build image
FROM ubuntu:latest
LABEL Description="Build environment for Helen"

# TODO: this needs Java and Maven
RUN apt-get update && apt-get -y install \
    cmake \
    g++ \
    git \
    && rm -rf /var/lib/apt/lists/*

WORKDIR /helen
COPY . /helen
COPY --from=0 /helen/dist /helen/priv/www/assets
RUN mv /helen/priv/www/assets/index.html /helen/priv/www
RUN mv /helen/priv/www/assets/favicon.ico /helen/priv/www
RUN export LD_LIBRARY_PATH=$LD_LIBRARY_PATH:/usr/local/lib && \
    make all && \
    ./rebar3 as prod release

## Run image
FROM ubuntu:latest
LABEL Description="Helen"

# TODO: this needs Java
RUN apt-get update && apt-get -y install \
    && rm -rf /var/lib/apt/lists/*

# TODO: copy jar and assets
COPY --from=1 /helen/_build/prod/rel/helen /helen

# prepare for docker-compose, where athena is available from a virtual
# host named "athena"
# TODO: make this change in config.properties instead of sys.config
RUN sed -i -e "s/localhost/athena/g" /helen/releases/0.1.0/sys.config

# TODO: run jar instead of reltool
CMD export LD_LIBRARY_PATH=$LD_LIBRARY_PATH:/usr/local/lib && \
    /helen/bin/helen start && \
    while true; do /helen/bin/helen ping; sleep 60; done

EXPOSE 8080

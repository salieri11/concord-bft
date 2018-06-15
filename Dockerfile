# This Dockerfile does not build Helen, it just dockerizes it.
# You must build the UI and the HTTP Server first (see Readme.md).

## Run image
FROM openjdk:8
LABEL Description="Helen"

WORKDIR /
COPY priv priv
COPY config.properties .
COPY helen*.jar .

# prepare for docker-compose, where athena is available from a virtual
# host named "athena1"
RUN sed -i -e "s/AthenaAuthorities=.*/AthenaAuthorities=athena1:5458,athena2:5459,athena3:5460/g" config.properties

CMD java -jar helen*.jar

EXPOSE 8080

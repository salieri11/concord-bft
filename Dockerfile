# This Dockerfile does not build Helen, it just dockerizes it.
# You must build the UI and the HTTP Server first (see Readme.md).
# Also, helen needs cockroach DB to run, when running helen inside a
# container, we should also start another container for cockroach DB.
# The `run_cockroachdb_docker.sh` script starts a cockroachDB container
# and configures it for helen. Run that script first before starting helen
# containers.

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

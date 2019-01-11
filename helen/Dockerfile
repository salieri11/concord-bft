# This Dockerfile does not build Helen, it just dockerizes it.
# You must build the UI and the HTTP Server first (see Readme.md).
# Also, helen needs cockroach DB to run, when running helen inside a
# container, we should also start another container for cockroach DB.
# The `run_cockroachdb_docker.sh` script starts a cockroachDB container
# and configures it for helen. Run that script first before starting helen
# containers.

## Image to build dependencies
FROM vmwblockchain/concord-ui:prereqs-v2
LABEL Description="Helen"

WORKDIR /
COPY src/main/resources/application.properties .
COPY target/blockchain-helen*.jar .

# prepare for docker-compose, where concord is available from a virtual
# hosts named "concord1", "concord2", ...
RUN sed -i -e "s/ConcordAuthorities=.*/ConcordAuthorities=concord1:5458,concord2:5458,concord3:5458,concord4:5458/g" application.properties
# and CockroachDB is available from a virtual host named db-server
RUN sed -i -e "s/DB_IP=.*/DB_IP=db-server/g" application.properties

CMD java -jar blockchain-helen*.jar

EXPOSE 8080

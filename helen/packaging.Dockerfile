# Dockerize locally pre-built Ethreaum RPC server artifact.
## Runtime image.
FROM vmwblockchain/concord-ui:prereqs-v2
LABEL description="Blockchain Management Service (Helen)"

WORKDIR /helen
COPY ./src/main/resources/application.properties ./application.properties
COPY ./target/blockchain-helen*.jar ./blockchain-helen.jar

RUN sed -i -e "s/ConcordAuthorities=.*/ConcordAuthorities=concord1:5458,concord2:5458,concord3:5458,concord4:5458/g" ./application.properties
# and CockroachDB is available from a virtual host named db-server
RUN sed -i -e "s/DB_IP=.*/DB_IP=db-server/g" ./application.properties

CMD ["java", "-jar", "blockchain-helen.jar"]

EXPOSE 8080

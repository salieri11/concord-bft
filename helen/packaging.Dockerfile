# Dockerize locally pre-built Ethreaum RPC server artifact.
## Runtime image.
FROM vmwblockchain/concord-ui:prereqs-v2
LABEL description="Blockchain Management Service (Helen)"

WORKDIR /helen
COPY ./src/main/resources/application.properties ./application.properties
COPY ./target/blockchain-helen*.jar ./blockchain-helen.jar

RUN  echo "ConcordAuthorities=concord1:5458,concord2:5458,concord3:5458,concord4:5458" > ./application-test.properties
# and CockroachDB is available from a virtual host named db-server
RUN echo "DB_IP=db-server" >> ./application-test.properties

CMD ["java", "-Dspring.profiles.active=test", "-jar", "blockchain-helen.jar"]

EXPOSE 8080

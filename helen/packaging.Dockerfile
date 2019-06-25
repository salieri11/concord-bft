# Dockerize locally pre-built Ethreaum RPC server artifact.
## Runtime image.
FROM athena-docker-local.artifactory.eng.vmware.com/helen:prereqs-v2
LABEL description="Blockchain Management Service (Helen)"

# env variable for parameterizing application profile
ENV APPCONTEXT test

# config volume for exposing app profiles
VOLUME ["/config"]

WORKDIR /helen
COPY ./src/main/resources/application.properties ./application.properties
COPY ./src/main/resources/database/schema.sql ./db-schema.sql
COPY ./target/blockchain-helen*.jar ./blockchain-helen.jar

CMD ["java", "-Dspring.config.location=/config/app/profiles/,./", "-Dspring.profiles.active=${APPCONTEXT}", "-jar", "blockchain-helen.jar"]

EXPOSE 8080

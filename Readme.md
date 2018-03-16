# Helen

Helen is Athena's interface to launch requests. This repository will be the home of the API server for Project Athena. Helen is a Java Undertow server which invokes different servlets for different apis. Note : Helen will run on pot 8080 of localhost.

### Prerequisites

[Java 8](http://www.oracle.com/technetwork/java/javase/downloads/jre8-downloads-2133155.html)
[Maven](https://www.rosehosting.com/blog/how-to-install-maven-on-ubuntu-16-04/)
[Athena](https://github.com/vmwathena/athena)

### Building and Running

 * Install dependencies and build using maven.

```
mvn clean install
```

 * Run Athena. Steps can be found [here](https://github.com/vmwathena/athena)


 * Run the server

```
mvn exec:java
```

 * Point your browser to localhost:8080/{some api}

## API

 * /static/ - Used to serve static resources.
 * /api/athena/members - Used to send a peer request to Athena. Client receives response in JSON.


# Helen

Helen is Athena's interface to launch requests. This repository will be the home of the API server for Project Athena. Helen is a Java Undertow server which invokes different servlets for different apis. Note : Helen will run on port 32773 of localhost by default.

### Prerequisites

[Java 8](http://www.oracle.com/technetwork/java/javase/downloads/jre8-downloads-2133155.html)

[Maven](https://www.rosehosting.com/blog/how-to-install-maven-on-ubuntu-16-04/)

[Athena](https://github.com/vmwathena/athena)

### macOS Setup

Install Java from the link above in the prerequisites, but maven can be installed via brew.

```
brew install boost aclocal protobuf@2.5 automake autoconf maven
```

Boost and protobuf won't be automatically linked

```
export DYLD_LIBRARY_PATH=/usr/local/Cellar/boost/1.66.0/lib:$DYLD_LIBRARY_PATH
export DYLD_LIBRARY_PATH=/usr/local/Cellar/protobuf@2.5/2.5.0/lib:$DYLD_LIBRARY_PATH
```

### Buiding and running the UI

You will need Node installed to build the UI. The project targets the current LTS version of Node (8.9.x). You can install Node directly, or by using NVM.

If you are not utilizing NVM, please skip any commands that reference it.

Before completing any of the following commands, make sure you are in the webapp/ directory and have the correct version of Node activated:

```
cd webapp/
nvm use
```

Also required is the Angular CLI, which currently handles asset compilation, component generation, and provides a local development server. Install using the following commands:

```
npm install -g @angular/cli
```

Install all dependencies through NPM.

```
npm install
```

Build the UI to webapp/dist/. This is primarily used in the Docker build process:

```
npm run build
```

Build and copy the UI in development mode to the priv/www/ directory for use on a locally running server:

```
npm run build:local:dev
```

Build and copy the UI in production mode to the priv/www/ directory for use on a locally running server:

```
npm run build:local:prod
```

Start the UI in watch mode, typically used for local development. This will:

    Start a server listening on http://localhost:4200 to deliver the UI assets.

    Proxy all API requests to an existing Helen server. Edit webapp/proxy.conf.json and make sure target matches your desired server and port.

    Watch for file changes under the webapp/ and compile automatically.

```
npm start
```

### Building and Running Helen

 * Run Athena. Steps can be found [here](https://github.com/vmwathena/athena)

 * Install dependencies and build using maven.

    
   To build only with unit tests (No Athena instance is needed) :
```
mvn clean install
```

   To build with unit and integration tests : (Athena must be running for this)
```
mvn clean install -DskipIntegrationTests=false
```

 * Run the server

```
mvn exec:java
```

### API

 * /swagger/* - Used to serve the static content from priv/www/swagger

 * /assets/* - Used to serve static content from priv/www/assets

 * /api and /api/ - Used to return a list of all other APIs serviced

 * /* - Used to serve content from priv/www/index.html

### Using the UI

 * Point the browser to localhost:32773 to load the dashboard.

 * Point the browser to localhost:32773/swagger/index.html to load the swagger UI.

### Configurations

Server and servlet configurations are stored in a key-value format in [config.properties](https://github.com/vmwathena/helen/blob/jmc-java-helen/config.properties).
Note : Helen needs to be restarted if any changes are made to the config.properties file.


/**
 * Server class for Helen. Boots up an Undertow server and multiple servlets.
 * Runs on localhost and port 8080 by default.
 * TODO : Read port from config file.
 * 
 * Helen connects to Athena at the backend. Communication between Helen and 
 * Athena is via a TCP socket connection. Messages are sent in Google Protocol 
 * Buffer format. Responses from Helen to the client are in Json format.
 * 
 * StaticContent serves static content located in a folder of the same name.
 * Members serves Peer Requests from Athena.
 */
package Servers;

import static io.undertow.servlet.Servlets.deployment;
import java.io.File;
import javax.servlet.ServletException;

import Servlets.MemberClass;
import Servlets.StaticContent;
import io.undertow.Handlers;
import io.undertow.Undertow;
import io.undertow.server.handlers.PathHandler;
import io.undertow.server.handlers.resource.FileResourceManager;
import io.undertow.servlet.Servlets;
import io.undertow.servlet.api.DeploymentInfo;
import io.undertow.servlet.api.DeploymentManager;

public class Server {
    private static final String PATH = "/";
    private static final String DEPLOYMENT_NAME = "helena.war";
    private static final String SERVLET1_NAME = "Members";
    private static final String SERVLET2_NAME = "StaticContent";
    private static final int PORT = 8080;

    public static void main(String[] args) throws Exception {
        try {
            DeploymentInfo servletBuilder = deployment()
                    .setClassLoader(Server.class.getClassLoader())
                    .setContextPath(PATH)
                    .setResourceManager(new FileResourceManager(
                            new File("index.html"), 1024))
                    .setDeploymentName(DEPLOYMENT_NAME)
                    .addServlets(
                            Servlets.servlet(SERVLET1_NAME, MemberClass.class)
                                    .addMapping("/api/athena/members"))
                    .addServlets(
                            Servlets.servlet(SERVLET2_NAME, StaticContent.class)
                                    .addMapping("/static/*"));
            DeploymentManager manager = Servlets.defaultContainer()
                    .addDeployment(servletBuilder);
            manager.deploy();

            PathHandler path = Handlers.path(Handlers.redirect(PATH))
                    .addPrefixPath(PATH, manager.start());
            Undertow server = Undertow.builder()
                    .addHttpListener(PORT, "0.0.0.0").setHandler(path).build();
            server.start();
        } catch (ServletException e) {
            throw new RuntimeException(e);
        }
    }
}
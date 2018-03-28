/**
 * Server class for Helen. Boots up an Undertow server and multiple servlets.
 * Runs on localhost and port 8080 by default.
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
import java.io.IOException;
import java.text.SimpleDateFormat;
import java.util.Date;
import javax.servlet.ServletException;
import org.apache.log4j.Logger;

import Servlets.BlockList;
import Servlets.BlockNumber;
import Servlets.DefaultContent;
import Servlets.MemberList;
import Servlets.StaticContent;
import configurations.FileConfiguration;
import configurations.IConfiguration;
import connections.AthenaConnectionPool;
import io.undertow.Handlers;
import io.undertow.Undertow;
import io.undertow.server.handlers.PathHandler;
import io.undertow.server.handlers.resource.FileResourceManager;
import io.undertow.servlet.Servlets;
import io.undertow.servlet.api.DeploymentInfo;
import io.undertow.servlet.api.DeploymentManager;

public class Server {
   private static String serverPath;
   private static String deploymentName;
   private static String memberListServletName;
   private static String staticContentServletName;
   private static String defaultContentServletName;
   private static String blockListServletName;
   private static String blockNumberServletName;
   private static String serverHostName;
   private static String defaultReponse;
   private static String staticContentEndpoint;
   private static String memberListEndpoint;
   private static String defaultContentEndpoint;
   private static String blockListEndpoint;
   private static String blockNumberEndpoint;
   private static int port;

   // Set current datetime for logging purposes
   static {
      SimpleDateFormat dateFormat = 
            new SimpleDateFormat("dd-MM-yyyy-hh-mm-ss");
      System.setProperty(
            "current.date.time", dateFormat.format(new Date()));
   }

   public static void main(String[] args) 
         throws IOException, ServletException {
      final Logger logger = Logger.getLogger(Server.class);

      // Read configurations file
      IConfiguration conf;
      try {
         conf= FileConfiguration.getInstance();
      } catch (IOException e) {
         logger.error("Error in reading configurations");
         throw new IOException();
      }
      
      serverPath =
            conf.getStringValue("Undertow_Path");
      deploymentName = 
            conf.getStringValue("Deployment_Name");
      memberListServletName =
            conf.getStringValue("MemberList_ServletName");
      staticContentServletName =
            conf.getStringValue("StaticContent_ServletName");
      defaultContentServletName =
            conf.getStringValue("DefaultContent_ServletName");
      blockListServletName =
            conf.getStringValue("BlockList_ServletName");
      blockNumberServletName =
            conf.getStringValue("BlockNumber_ServletName");
      serverHostName =
            conf.getStringValue("Server_Host");
      port =
            conf.getIntegerValue("Server_Port");
      defaultReponse =
            conf.getStringValue("Server_DefaultResponse");
      staticContentEndpoint =
            conf.getStringValue("StaticContent_Endpoint");
      memberListEndpoint =
            conf.getStringValue("MemberList_Endpoint");
      defaultContentEndpoint =
            conf.getStringValue("DefaultContent_Endpoint");
      blockListEndpoint =
            conf.getStringValue("BlockList_Endpoint");
      blockNumberEndpoint =
            conf.getStringValue("BlockNumber_Endpoint");

      DeploymentInfo servletBuilder = deployment()
               .setClassLoader(Server.class.getClassLoader())
               .setContextPath(serverPath)
               .setResourceManager(
                        new FileResourceManager(
                              new File(defaultReponse), 1024))
               .setDeploymentName(deploymentName)
               .addServlets(Servlets
                        .servlet(memberListServletName,
                              MemberList.class)
                        .addMapping(memberListEndpoint))
               .addServlets(Servlets
                        .servlet(staticContentServletName,
                              StaticContent.class)
                        .addMapping(staticContentEndpoint))
               .addServlets(
                        Servlets.servlet(blockListServletName,
                              BlockList.class)
                         .addMapping(blockListEndpoint))
               .addServlets(Servlets
                        .servlet(blockNumberServletName,
                              BlockNumber.class)
                        .addMapping(blockNumberEndpoint))
               .addServlet(Servlets
                        .servlet(defaultContentServletName,
                                 DefaultContent.class)
                        .addMapping(defaultContentEndpoint));
      DeploymentManager manager = Servlets.defaultContainer()
               .addDeployment(servletBuilder);
      manager.deploy();

      PathHandler path;
      try {
         path = Handlers.path(Handlers.redirect(serverPath))
                  .addPrefixPath(serverPath, manager.start());
      } catch (ServletException e1) {
         logger.error("Error in starting Deployment Manager");
         throw new ServletException();
      }
      
      AthenaConnectionPool.getInstance().initialize(conf);
      
      Undertow server = Undertow.builder()
            .addHttpListener(port, serverHostName)
               .setHandler(path)
               // .setIoThreads(10)
               // to change number of io threads
               // .setWorkerThreads(10)
               //to change number of worker threads
               .build();
      server.start();
      logger.info("Server Booted");
   }
}
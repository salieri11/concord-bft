/**
 * Server class for Helen. Boots up an Undertow server and multiple servlets.
 * Runs on localhost and port 32773 (as per variables in config file).
 * 
 * Helen connects to Athena at the backend. Communication between Helen and 
 * Athena is via a TCP socket connection. Messages are sent in Google Protocol 
 * Buffer format. Responses from Helen to the client are in Json format.
 * 
 */
package Servers;

import static io.undertow.servlet.Servlets.deployment;
import java.io.File;
import java.io.IOException;
import java.text.SimpleDateFormat;
import java.util.Date;
import java.util.Properties;
import javax.servlet.ServletException;
import org.apache.log4j.Logger;
import org.json.simple.parser.ParseException;

import Servlets.ApiList;
import Servlets.Assets;
import Servlets.BlockList;
import Servlets.BlockNumber;
import Servlets.DefaultContent;
import Servlets.EthRPC;
import Servlets.MemberList;
import Servlets.Swagger;
import Servlets.Transaction;
import configurations.SystemConfiguration;
import connections.AthenaTCPConnection;
import io.undertow.Handlers;
import io.undertow.Undertow;
import io.undertow.server.handlers.PathHandler;
import io.undertow.server.handlers.resource.FileResourceManager;
import io.undertow.servlet.Servlets;
import io.undertow.servlet.api.DeploymentInfo;
import io.undertow.servlet.api.DeploymentManager;

public class Server {
   private static Properties config;
   private static String serverPath;
   private static String deploymentName;
   private static String serverHostName;
   private static int port;
   private static String defaultReponse;

   private static String memberListServletName;
   private static String defaultContentServletName;
   private static String blockListServletName;
   private static String blockNumberServletName;
   private static String ethRPCServletName;
   private static String transactionServletName;
   private static String swaggerServletName;
   private static String assetsServletName;
   private static String apiListServletName;

   private static String memberListEndpoint;
   private static String defaultContentEndpoint;
   private static String blockListEndpoint;
   private static String blockNumberEndpoint;
   private static String ethRPCEndpoint;
   private static String transactionEndpoint;
   private static String swaggerEndpoint;
   private static String assetsEndpoint;
   private static String apiListEndpoint;

   // Set current datetime for logging purposes
   static {
      SimpleDateFormat dateFormat = new SimpleDateFormat("dd-MM-yyyy-hh-mm-ss");
      System.setProperty("current.date.time", dateFormat.format(new Date()));
   }

   public static void main(String[] args)
            throws IOException, ServletException, ParseException {
      final Logger logger = Logger.getLogger(Server.class);

      // Read configurations file
      SystemConfiguration s;
      try {
         s = SystemConfiguration.getInstance();
      } catch (IOException e) {
         logger.error("Error in reading configurations");
         throw new IOException();
      }
      config = s.configurations;
      serverPath = config.getProperty("Undertow_Path");
      deploymentName = config.getProperty("Deployment_Name");
      serverHostName = config.getProperty("Server_Host");
      port = Integer.parseInt(config.getProperty("Server_Port"));
      defaultReponse = config.getProperty("Server_DefaultResponse");

      memberListServletName = config.getProperty("MemberList_ServletName");
      defaultContentServletName = config
               .getProperty("DefaultContent_ServletName");
      blockListServletName = config.getProperty("BlockList_ServletName");
      blockNumberServletName = config.getProperty("BlockNumber_ServletName");
      ethRPCServletName = config.getProperty("EthRPC_ServletName");
      transactionServletName = config.getProperty("Transaction_ServletName");
      swaggerServletName = config.getProperty("Swagger_ServletName");
      assetsServletName = config.getProperty("Assets_ServletName");
      apiListServletName = config.getProperty("ApiList_ServletName");

      memberListEndpoint = config.getProperty("MemberList_Endpoint");
      defaultContentEndpoint = config.getProperty("DefaultContent_Endpoint");
      blockListEndpoint = config.getProperty("BlockList_Endpoint");
      blockNumberEndpoint = config.getProperty("BlockNumber_Endpoint");
      ethRPCEndpoint = config.getProperty("EthRPC_Endpoint");
      transactionEndpoint = config.getProperty("Transaction_Endpoint");
      swaggerEndpoint = config.getProperty("Swagger_Endpoint");
      assetsEndpoint = config.getProperty("Assets_Endpoint");
      apiListEndpoint = config.getProperty("ApiList_Endpoint");

      DeploymentInfo servletBuilder = deployment()
               .setClassLoader(Server.class.getClassLoader())
               .setContextPath(serverPath)
               .setResourceManager(
                        new FileResourceManager(new File(defaultReponse), 1024))
               .setDeploymentName(deploymentName)
               .addServlets(Servlets
                        .servlet(memberListServletName, MemberList.class)
                        .addMapping(memberListEndpoint))
               .addServlets(Servlets.servlet(swaggerServletName, Swagger.class)
                        .addMapping(swaggerEndpoint))
               .addServlets(Servlets.servlet(assetsServletName, Assets.class)
                        .addMapping(assetsEndpoint))
               .addServlets(Servlets.servlet(apiListServletName, ApiList.class)
                        .addMapping(apiListEndpoint))
               .addServlets(
                        Servlets.servlet(blockListServletName, BlockList.class)
                                 .addMapping(blockListEndpoint))
               .addServlets(Servlets
                        .servlet(blockNumberServletName, BlockNumber.class)
                        .addMapping(blockNumberEndpoint))
               .addServlets(Servlets.servlet(ethRPCServletName, EthRPC.class)
                        .addMapping(ethRPCEndpoint))
               .addServlets(Servlets
                        .servlet(transactionServletName, Transaction.class)
                        .addMapping(transactionEndpoint))
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
      Undertow server = Undertow.builder().addHttpListener(port, serverHostName)
               .setHandler(path)
               // .setIoThreads(10) // to change number of io threads
               // .setWorkerThreads(10) //to change number of worker threads
               .build();
      server.start();
      logger.info("Server Booted");

      /*
       * Indirectly invokes constructor of singleton class which establishes a
       * TCP connection with Athena.
       */
      try {
         AthenaTCPConnection.getInstance();
      } catch (IOException e) {
         logger.error("Error in establishing TCP connection with Athena");
         throw new IOException();
      }
   }
}
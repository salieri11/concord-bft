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
import javax.servlet.ServletException;
import org.apache.log4j.Logger;
import Servlets.BlockList;
import Servlets.BlockNumber;
import Servlets.EthRPC;
import Servlets.MemberList;
import Servlets.StaticContent;
import configurations.ConfigurationFactory;
import configurations.FileConfiguration;
import configurations.IConfiguration;
import configurations.ConfigurationFactory.ConfigurationType;
import connections.AthenaConnectionFactory;
import connections.AthenaConnectionFactory.ConnectionType;
import connections.AthenaConnectionPool;
import Servlets.Transaction;
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
   private static String memberListEndpoint1;
   private static String memberListEndpoint2;
   private static String defaultContentEndpoint;
   private static String blockListEndpoint1;
   private static String blockListEndpoint2;
   private static String blockNumberEndpoint;
   private static String ethRPCEndpoint1;
   private static String ethRPCEndpoint2;
   private static String transactionEndpoint;
   private static String swaggerEndpoint;
   private static String assetsEndpoint;
   private static String apiListEndpoint;

   // Set current datetime for logging purposes
   static {
      SimpleDateFormat dateFormat = new SimpleDateFormat("dd-MM-yyyy-hh-mm-ss");
      System.setProperty("current.date.time", dateFormat.format(new Date()));
   }

   public static void main(String[] args) throws IOException, ServletException {

      final Logger logger = Logger.getLogger(Server.class);

      // Read configurations file
      IConfiguration conf
         = ConfigurationFactory.getConfiguration(ConfigurationType.File);

      serverPath = conf.getStringValue("Undertow_Path");
      deploymentName = conf.getStringValue("Deployment_Name");
      serverHostName = conf.getStringValue("Server_Host");
      port = conf.getIntegerValue("Server_Port");
      defaultReponse = conf.getStringValue("Server_DefaultResponse");

      memberListServletName = conf.getStringValue("MemberList_ServletName");
      defaultContentServletName
         = conf.getStringValue("DefaultContent_ServletName");
      blockListServletName = conf.getStringValue("BlockList_ServletName");
      blockNumberServletName = conf.getStringValue("BlockNumber_ServletName");
      ethRPCServletName = conf.getStringValue("EthRPC_ServletName");
      transactionServletName = conf.getStringValue("Transaction_ServletName");
      swaggerServletName = conf.getStringValue("Swagger_ServletName");
      assetsServletName = conf.getStringValue("Assets_ServletName");
      apiListServletName = conf.getStringValue("ApiList_ServletName");

      memberListEndpoint1 = conf.getStringValue("MemberList_Endpoint1");
      memberListEndpoint2 = conf.getStringValue("MemberList_Endpoint2");
      defaultContentEndpoint = conf.getStringValue("DefaultContent_Endpoint");
      blockListEndpoint1 = conf.getStringValue("BlockList_Endpoint1");
      blockListEndpoint2 = conf.getStringValue("BlockList_Endpoint2");
      blockNumberEndpoint = conf.getStringValue("BlockNumber_Endpoint");
      ethRPCEndpoint1 = conf.getStringValue("EthRPC_Endpoint1");
      ethRPCEndpoint2 = conf.getStringValue("EthRPC_Endpoint2");
      transactionEndpoint = conf.getStringValue("Transaction_Endpoint");
      swaggerEndpoint = conf.getStringValue("Swagger_Endpoint");
      assetsEndpoint = conf.getStringValue("Assets_Endpoint");
      apiListEndpoint = conf.getStringValue("ApiList_Endpoint");

      DeploymentInfo servletBuilder
         = deployment().setClassLoader(Server.class.getClassLoader())
                       .setContextPath(serverPath)
                       .setResourceManager(
                                           // 1024 : Size to use direct FS to
                                           // network transfer
                                           // (if supported by OS/JDK) instead
                                           // of read/write
                                           new FileResourceManager(new File(defaultReponse),
                                                                   1024))
                       .setDeploymentName(deploymentName)
                       .addServlets(Servlets.servlet(memberListServletName,
                                                     MemberList.class)
                                            .addMapping(memberListEndpoint1)
                                            .addMapping(memberListEndpoint2))
                       .addServlets(Servlets.servlet(swaggerServletName,
                                                     StaticContent.class)
                                            .addMapping(swaggerEndpoint))
                       .addServlets(Servlets.servlet(assetsServletName,
                                                     StaticContent.class)
                                            .addMapping(assetsEndpoint))
                       .addServlets(Servlets.servlet(apiListServletName,
                                                     StaticContent.class)
                                            .addMapping(apiListEndpoint))
                       .addServlets(Servlets.servlet(blockListServletName,
                                                     BlockList.class)
                                            .addMapping(blockListEndpoint1)
                                            .addMapping(blockListEndpoint2))
                       .addServlets(Servlets.servlet(blockNumberServletName,
                                                     BlockNumber.class)
                                            .addMapping(blockNumberEndpoint))
                       .addServlets(Servlets.servlet(ethRPCServletName,
                                                     EthRPC.class)
                                            .addMapping(ethRPCEndpoint1)
                                            .addMapping(ethRPCEndpoint2))
                       .addServlets(Servlets.servlet(transactionServletName,
                                                     Transaction.class)
                                            .addMapping(transactionEndpoint))
                       .addServlet(Servlets.servlet(defaultContentServletName,
                                                    StaticContent.class)
                                           .addMapping(defaultContentEndpoint));
      DeploymentManager manager
         = Servlets.defaultContainer().addDeployment(servletBuilder);
      manager.deploy();

      PathHandler path;
      try {
         path = Handlers.path(Handlers.redirect(serverPath))
                        .addPrefixPath(serverPath, manager.start());
      } catch (ServletException e1) {
         logger.error("Error in starting Deployment Manager");
         throw e1;
      }

      // Initialize the connection pool
      AthenaConnectionFactory factory
         = new AthenaConnectionFactory(ConnectionType.TCP, conf);
      AthenaConnectionPool.getInstance().initialize(conf, factory);
      logger.info("athena connection pool initialized");

      Undertow server = Undertow.builder()
                                .addHttpListener(port, serverHostName)
                                .setHandler(path)
                                // .setIoThreads(10) // to change number of io
                                // threads
                                // .setWorkerThreads(10) //to change number of
                                // worker threads
                                .build();
      server.start();
      logger.info("Server Booted");
   }
}

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

import javax.persistence.Entity;
import javax.servlet.ServletException;

import org.apache.log4j.Logger;

import Servlets.*;
import configurations.ConfigurationFactory;
import configurations.ConfigurationFactory.ConfigurationType;
import configurations.IConfiguration;
import connections.AthenaConnectionFactory;
import connections.AthenaConnectionFactory.ConnectionType;
import connections.AthenaConnectionPool;
import io.undertow.Handlers;
import io.undertow.Undertow;
import io.undertow.server.handlers.PathHandler;
import io.undertow.server.handlers.resource.FileResourceManager;
import io.undertow.servlet.Servlets;
import io.undertow.servlet.api.DeploymentInfo;
import io.undertow.servlet.api.DeploymentManager;
import org.springframework.boot.SpringApplication;
import org.springframework.boot.autoconfigure.EnableAutoConfiguration;
import org.springframework.boot.autoconfigure.SpringBootApplication;
import org.springframework.boot.autoconfigure.domain.EntityScan;
import org.springframework.context.ApplicationContext;
import org.springframework.context.annotation.ComponentScan;
import org.springframework.data.jpa.repository.config.EnableJpaRepositories;


@SpringBootApplication
@EntityScan("profiles")
@EnableJpaRepositories("profiles")
@ComponentScan(basePackages = {"profiles", "Servlets"})
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
   private static String ethDispatcherServletName;
   private static String transactionServletName;
   private static String transactionListServletName;
   private static String swaggerServletName;
   private static String assetsServletName;
   private static String apiListServletName;
   private static String memberListEndpoint;
   private static String defaultContentEndpoint;
   private static String blockListEndpoint;
   private static String blockNumberEndpoint;
   private static String ethRPCEndpoint;
   private static String transactionEndpoint;
   private static String transactionListEndpoint;
   private static String swaggerEndpoint;
   private static String assetsEndpoint;
   private static String apiListEndpoint;
   private static String contractServletName;
   private static String contractEndpoint;
   private static String userManagementServletName;
   private static String userManagementEndpoint;

   // Set current datetime for logging purposes
   static {
      SimpleDateFormat dateFormat = new SimpleDateFormat("dd-MM-yyyy-hh-mm-ss");
      System.setProperty("current.date.time", dateFormat.format(new Date()));
   }

   public static void main(String[] args) throws IOException, ServletException {
      final Logger logger = Logger.getLogger(Server.class);
      if (args.length == 1) {
         // This accepts only 1 argument and it is name of configuration file
         ConfigurationFactory.init(args[0]);
      } else {
         ConfigurationFactory.init();
      }

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
      ethDispatcherServletName
         = conf.getStringValue("EthDispatcher_ServletName");
      transactionServletName = conf.getStringValue("Transaction_ServletName");
      transactionListServletName
         = conf.getStringValue("TransactionList_ServletName");
      swaggerServletName = conf.getStringValue("Swagger_ServletName");
      assetsServletName = conf.getStringValue("Assets_ServletName");
      apiListServletName = conf.getStringValue("ApiList_ServletName");

      memberListEndpoint
         = removeTrailingSlash(conf.getStringValue("MemberList_Endpoint"));
      defaultContentEndpoint = conf.getStringValue("DefaultContent_Endpoint");
      blockListEndpoint
         = removeTrailingSlash(conf.getStringValue("BlockList_Endpoint"));
      blockNumberEndpoint = conf.getStringValue("BlockNumber_Endpoint");
      ethRPCEndpoint
         = removeTrailingSlash(conf.getStringValue("EthRPC_Endpoint"));
      transactionEndpoint = conf.getStringValue("Transaction_Endpoint");
      transactionListEndpoint = conf.getStringValue("TransactionList_Endpoint");
      swaggerEndpoint = conf.getStringValue("Swagger_Endpoint");
      assetsEndpoint = conf.getStringValue("Assets_Endpoint");
      apiListEndpoint = conf.getStringValue("ApiList_Endpoint");
      contractServletName = conf.getStringValue("Contracts_ServletName");
      contractEndpoint = conf.getStringValue("Contracts_Endpoint");
      userManagementServletName
         = conf.getStringValue("UserManagement_ServletName");
      userManagementEndpoint = conf.getStringValue("UserManagement_Endpoint");

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
                                            .addMapping(memberListEndpoint)
                                            .addMapping(memberListEndpoint
                                               + '/'))
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
                                            .addMapping(blockListEndpoint)
                                            .addMapping(blockListEndpoint
                                               + '/'))
                       .addServlets(Servlets.servlet(blockNumberServletName,
                                                     BlockNumber.class)
                                            .addMapping(blockNumberEndpoint))
                       .addServlets(Servlets.servlet(ethDispatcherServletName,
                                                     EthDispatcher.class)
                                            .addMapping(ethRPCEndpoint)
                                            .addMapping(ethRPCEndpoint + '/'))
                       .addServlets(Servlets.servlet(transactionServletName,
                                                     Transaction.class)
                                            .addMapping(transactionEndpoint)
                                            .addMapping(transactionEndpoint
                                               + '/'))
                       .addServlets(Servlets.servlet(transactionListServletName,
                                                     TransactionList.class)
                                            .addMapping(transactionListEndpoint)
                                            .addMapping(transactionListEndpoint
                                               + '/'))
                       .addServlet(Servlets.servlet(defaultContentServletName,
                                                    StaticContent.class)
                                           .addMapping(defaultContentEndpoint))
                       .addServlet(Servlets.servlet(contractServletName,
                                                    ContractsServlet.class)
                                           .addMapping(contractEndpoint)
                                           .addMapping(contractEndpoint + "/*"))
                       .addServlet(Servlets.servlet(userManagementServletName,
                                                    ProfileManager.class)
                                           .addMapping(userManagementEndpoint)
                                           .addMapping(userManagementEndpoint
                                              + "/*"));

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
                                // to change number of io threads
                                // .setIoThreads(10)
                                // to change number of worker threads
                                // .setWorkerThreads(10)
                                .build();
      server.start();
      logger.info("Server Booted");
   
      SpringApplication.run(Server.class, args);
   }

   /**
    * Removes trailing '/' character
    *
    * @param api
    * @return
    */
   private static String removeTrailingSlash(String api) {
      String result = api;
      if (result != null) {
         int len = result.length();
         if (len > 0) {
            if (result.charAt(len - 1) == '/') {
               result = result.substring(0, len - 1);
            }
         }
      }
      return result;
   }
}

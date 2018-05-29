package database;

import java.sql.Connection;
import java.sql.DriverManager;

import org.apache.log4j.Logger;

import configurations.ConfigurationFactory;
import configurations.IConfiguration;

/**
 * <p>
 * Copyright 2018 VMware, all rights reserved.
 * </p>
 *
 * A class which manages and provides access to database Connection objects.
 */
public class DatabaseService {

   // TODO: create a pool of connection objects rather than using just a single
   // object
   private static Connection db;
   private static Logger logger = Logger.getLogger(DatabaseService.class);
   private static IConfiguration _conf;
   static {
      _conf
         = ConfigurationFactory.getConfiguration(ConfigurationFactory.ConfigurationType.File);
      try {
         String url = _conf.getStringValue("DB_PROTOCOL") + "://"
            + _conf.getStringValue("DB_IP") + ":"
            + _conf.getStringValue("DB_PORT") + "/"
            + _conf.getStringValue("DB_NAME") + "?"
            + _conf.getStringValue("DB_OPTIONS");
         logger.debug("Connecting to database at: " + url);
         db = DriverManager.getConnection(url,
                                          _conf.getStringValue("DB_USER"),
                                          _conf.getStringValue("DB_PASSWORD"));
      } catch (Exception e) {
         logger.fatal("Error in initializing database: ", e);
         // TODO: is there a better way than doing system.exit?
         System.exit(0);
      }
   }

   public static Connection getDatabaseConnection() {
      return db;
   }
}

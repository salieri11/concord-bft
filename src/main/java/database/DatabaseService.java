package database;

import java.sql.Connection;
import java.sql.DriverManager;

import configurations.ConfigurationFactory;
import configurations.IConfiguration;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

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
   private static Connection db = null;
   private static boolean initDone = false;
   private static Logger logger = LogManager.getLogger(DatabaseService.class);
   private static IConfiguration _conf;

   private static void init() throws Exception {
      _conf
         = ConfigurationFactory.getConfiguration(ConfigurationFactory.ConfigurationType.File);
      String url = _conf.getStringValue("DB_PROTOCOL") + "://"
         + _conf.getStringValue("DB_IP") + ":" + _conf.getStringValue("DB_PORT")
         + "/" + _conf.getStringValue("DB_NAME") + "?"
         + _conf.getStringValue("DB_OPTIONS");
      logger.debug("Connecting to database at: " + url);
      db = DriverManager.getConnection(url,
                                       _conf.getStringValue("DB_USER"),
                                       _conf.getStringValue("DB_PASSWORD"));
   }

   public static synchronized Connection
          getDatabaseConnection() throws ServiceUnavailableException {
      if (!initDone) {
         try {
            init();
         } catch (Exception e) {
            logger.error(e);
         }
         initDone = true;
      }

      if (initDone && db == null) {
         throw new ServiceUnavailableException("Database service is not "
            + "available");
      }

      return db;
   }
}

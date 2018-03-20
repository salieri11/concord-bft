/**
 * This singleton class is used to maintain common system configuration
 * resources.
 * 
 * A config.properties file contains the configrations in key value pair 
 * format. This class provides a handle to the file.
 */
package configurations;

import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.io.InputStream;
import java.util.Properties;

import org.apache.log4j.Logger;

public final class SystemConfiguration {
   private static SystemConfiguration single_instance = null;
   public Properties configurations;
   private static Logger logger;

   /**
    * Loads the configurations file
    * 
    * @throws IOException
    */
   private SystemConfiguration() throws IOException {
      logger = Logger.getLogger(SystemConfiguration.class);
      configurations = new Properties();
      InputStream input = null;

      try {
         input = new FileInputStream("config.properties");
      } catch (FileNotFoundException e) {
         logger.error("Error reading config file");
         throw new FileNotFoundException();
      }
      try {
         configurations.load(input);
      } catch (IOException e) {
         logger.error("Error loading config file");
         throw new IOException();
      }
   }

   /**
    * Static method to create/retrieve the instance of this class
    * 
    * @return Instance of this class
    * @throws IOException
    */
   public static SystemConfiguration getInstance() throws IOException {
      if (single_instance == null) {
         try {
            single_instance = new SystemConfiguration();
         } catch (IOException e) {
            logger.error("Error in creating new System Configuration object");
            throw new IOException();
         }
      }
      return single_instance;
   }
}

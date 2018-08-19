/**
 * This singleton class is used to maintain common system configuration
 * resources.
 * 
 * A application.properties file contains the configrations in key value pair
 * format. This class provides a handle to the file.
 */
package configurations;

import java.io.FileInputStream;
import java.io.IOException;
import java.util.Properties;
import org.apache.logging.log4j.LogManager;

import org.apache.logging.log4j.Logger;
import org.json.simple.parser.ParseException;

public class FileConfiguration implements IConfiguration {

   protected Properties _configurations;
   protected Logger logger = LogManager.getRootLogger();

   /**
    * Loads default application.properties file
    * 
    * @throws IOException
    * @throws ParseException
    */
   protected FileConfiguration() throws IOException {
      this("application.properties");
   }

   /**
    * Loads the given configurations file
    * 
    * @throws IOException
    * @throws ParseException
    **/
   protected FileConfiguration(String propertiesFile) throws IOException {
      _configurations = new Properties();
      try (FileInputStream fi = new FileInputStream(propertiesFile)) {
         _configurations.load(fi);
      }
   }

   /**
    * Used to read an integer configuration
    */
   @Override
   public int getIntegerValue(String key) {
      int retval = Integer.parseInt(_configurations.getProperty(key));
      return retval;
   }

   /**
    * Used to read a string configuration
    */
   @Override
   public String getStringValue(String key) {
      String retval = _configurations.getProperty(key);
      return retval;
   }

   /**
    * Used to read a long configuration
    */
   @Override
   public long getLongValue(String key) {
      long retval = Long.parseLong(_configurations.getProperty(key));
      return retval;
   }
}
/**
 * This singleton class is used to maintain common system configuration
 * resources.
 * 
 * A config.properties file contains the configrations in key value pair format.
 * This class provides a handle to the file.
 */
package configurations;

import java.io.*;
import java.nio.Buffer;
import java.util.Properties;

import org.apache.log4j.Logger;
import org.json.simple.parser.ParseException;

public class FileConfiguration implements IConfiguration {

   protected Properties _configurations;
   private Logger logger = Logger.getLogger(FileConfiguration.class);

   /**
    * Loads default config.properties file
    * 
    * @throws IOException
    * @throws ParseException
    */
   protected FileConfiguration() throws
           IOException {
      this("application.properties");
   }

   /**
    * Loads the given configurations file
    * 
    * @throws IOException
    * @throws ParseException
    **/
   protected FileConfiguration(String propertiesFile) throws
           IOException {
      _configurations = new Properties();
      try (InputStream ins = getClass()
              .getResourceAsStream("/" + propertiesFile);
           BufferedReader br = new BufferedReader(new InputStreamReader(ins))) {
         _configurations.load(br);
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
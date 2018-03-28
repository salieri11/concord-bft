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

public final class FileConfiguration implements IConfiguration {
   private static FileConfiguration _single_instance =
            new FileConfiguration();
   private Properties _configurations;
   private static Logger _logger =
         Logger.getLogger(FileConfiguration.class);
   
   /**
    * Loads the configurations file
    * 
    * @throws IOException
   */
   private FileConfiguration() {
      _configurations = new Properties();
      InputStream input = null;
   
      try {
         input = new FileInputStream("config.properties");
      } catch (FileNotFoundException e) {
         _logger.error("Error reading config file");
      }
      try {
         _configurations.load(input);
      } catch (IOException e) {
         _logger.error("Error loading config file");
      }
   }
   
   /**
    * Static method to create/retrieve the instance of this class
    * 
    * @return Instance of this class
    * @throws IOException
   */
   public static FileConfiguration getInstance() throws IOException {
         return _single_instance;
   }
   
      @Override
   public int getIntegerValue(String key) {
      int retval = Integer.parseInt(_configurations.getProperty(key));
      return retval;
   }
   
   @Override
   public String getStringValue(String key) {
      String retval = _configurations.getProperty(key);
      return retval;
   }
}
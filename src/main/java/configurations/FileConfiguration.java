/**
* This singleton class is used to maintain common system configuration
 * resources.
 * 
 * A config.properties file contains the configrations in key value pair 
 * format. This class provides a handle to the file.
 */
package configurations;

import java.io.FileInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.util.Properties;

import org.apache.log4j.Logger;
import org.json.simple.parser.ParseException;

public final class FileConfiguration implements IConfiguration {

   // Java way to eager initialize singleton with
   // ctor that throws exceptions. Static initializer block can throw
   // unchecked exception to class loader
   private static FileConfiguration _single_instance;
   static {
      try {
         _single_instance = new FileConfiguration();
      } catch (IOException e) {
         throw new ExceptionInInitializerError(e);
      } catch (ParseException e) {
         throw new ExceptionInInitializerError(e);
      }
   }

   private Properties _configurations;
   private static Logger _logger = Logger.getLogger(FileConfiguration.class);

   /**
    * Loads the configurations file
    * 
    * @throws IOException
    * @throws ParseException
    **/
   private FileConfiguration() throws IOException, ParseException {
      _configurations = new Properties();
      try (InputStream input = new FileInputStream("config.properties")) {
         _configurations.load(input);
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
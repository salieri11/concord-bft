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
import org.json.simple.JSONArray;
import org.json.simple.parser.JSONParser;
import org.json.simple.parser.ParseException;

public final class SystemConfiguration {
   private static SystemConfiguration single_instance = null;
   public Properties configurations;
   private static Logger logger;
   public JSONArray rpcList;

   /**
    * Loads the configurations file
    * 
    * @throws IOException
    * @throws ParseException
    */
   private SystemConfiguration() throws IOException, ParseException {
      logger = Logger.getLogger(SystemConfiguration.class);
      configurations = new Properties();

      try (InputStream input = new FileInputStream("config.properties");) {
         configurations.load(input);
      } catch (FileNotFoundException e) {
         logger.error("Error reading config file");
         throw e;
      }

      /*
       * Read the list of Eth RPCs (stored in a JSON Array format in the config
       * file)
       */
      JSONParser parser = new JSONParser();
      try {
         rpcList = (JSONArray) parser
                  .parse((String) configurations.getProperty("EthRPCList"));
      } catch (ParseException e) {
         logger.error("Error in parsing RPC List from configurations file");
         throw e;
      }
   }

   /**
    * Static method to create/retrieve the instance of this class
    * 
    * @return Instance of this class
    * @throws IOException
    * @throws ParseException
    */
   public static synchronized SystemConfiguration getInstance()
            throws IOException, ParseException {
      if (single_instance == null) {
         try {
            single_instance = new SystemConfiguration();
         } catch (IOException e) {
            logger.error("Error in creating new System Configuration object");
            throw e;
         }
      }
      return single_instance;
   }
}

/* this class can be used for unit testsing
 * it allow to override the existing configuration settings
 * during the runtime
 * 
 */

package configurations;

import java.io.IOException;
import org.json.simple.parser.ParseException;

public class TestConfiguration extends FileConfiguration {
   private static TestConfiguration _instance;
   static
   {
      try{
            _instance = new TestConfiguration();
         } catch (IOException e) {
            throw new ExceptionInInitializerError(e);
         } catch (ParseException e) {
            throw new ExceptionInInitializerError(e);
         }
   }
      
   private TestConfiguration() throws IOException, ParseException {
      super();
   }
   
   public static TestConfiguration getInstance() {
      return _instance;
   }

   public void setConnectionPoolSize(int value) {
      _configurations
         .setProperty("ConnectionPoolSize", String.valueOf(value));
   }
   
   public void setConnectionPoolFactor(int value) {
      _configurations
         .setProperty("ConnectionPoolFactor", String.valueOf(value));
   }
}
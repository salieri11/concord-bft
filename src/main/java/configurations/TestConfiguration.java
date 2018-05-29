/*
 * this class can be used for unit testsing it allow to override the existing
 * configuration settings during the runtime
 * 
 */

package configurations;

import java.io.IOException;
import org.json.simple.parser.ParseException;

public class TestConfiguration extends FileConfiguration {
   
   public TestConfiguration() {
      super();
   }

   public void setConnectionPoolSize(int value) {
      _configurations.setProperty("ConnectionPoolSize", String.valueOf(value));
   }

   public void setConnectionPoolFactor(int value) {
      _configurations.setProperty("ConnectionPoolFactor",
                                  String.valueOf(value));
   }
}
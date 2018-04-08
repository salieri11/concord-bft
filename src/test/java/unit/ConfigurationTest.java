package unit;

import java.io.IOException;

import base.BaseTest;
import configurations.TestConfiguration;

public class ConfigurationTest extends BaseTest {
   public ConfigurationTest() throws IOException {
      super(ConfigurationTest.class);
   }
   
   public void testOverrideConfiguration() {
      TestConfiguration tc = (TestConfiguration)_conf;
      tc.setConnectionPoolFactor(4);
      tc.setConnectionPoolSize(20);
      
      int poolFactor = tc.getIntegerValue("ConnectionPoolFactor");
      int poolSize = tc.getIntegerValue("ConnectionPoolSize");
      
      assertEquals(4, poolFactor);
      assertEquals(20, poolSize);
   }
}


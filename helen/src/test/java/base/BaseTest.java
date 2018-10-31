package base;

import java.io.IOException;

import configurations.ConfigurationFactory;
import configurations.ConfigurationFactory.ConfigurationType;
import configurations.IConfiguration;
import junit.framework.TestCase;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

public abstract class BaseTest extends TestCase {
   protected IConfiguration _conf;
   protected Logger _log;

   public BaseTest() {

   }

   @SuppressWarnings("rawtypes")
   protected BaseTest(Class clazz) throws IOException {
      ConfigurationFactory.init();
      _conf = ConfigurationFactory.getConfiguration(ConfigurationType.Test);
      _log = LogManager.getLogger(clazz);
   }
}

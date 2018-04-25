package base;

import java.io.IOException;

import org.apache.log4j.Logger;

import configurations.ConfigurationFactory;
import configurations.IConfiguration;
import configurations.ConfigurationFactory.ConfigurationType;
import junit.framework.*;

public abstract class BaseTest extends TestCase {
   protected IConfiguration _conf;
   protected Logger _log;

   public BaseTest() {

   }

   @SuppressWarnings("rawtypes")
   protected BaseTest(Class clazz) throws IOException {
      _conf = ConfigurationFactory.getConfiguration(ConfigurationType.Test);
      _log = Logger.getLogger(clazz);
   }
}

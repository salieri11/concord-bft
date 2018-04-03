package basic;

import java.io.IOException;

import org.apache.log4j.Logger;
import configurations.FileConfiguration;
import configurations.IConfiguration;
import junit.framework.*;

public abstract class BaseTest extends TestCase {
   protected IConfiguration _conf;;
   protected Logger _log;
   
   public BaseTest() {
      
   }
   
   protected BaseTest(Class clazz) throws IOException {
      _conf = FileConfiguration.getInstance();
      _log = Logger.getLogger(clazz);
   }
}

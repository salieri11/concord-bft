package basic;

import java.io.IOException;

import org.apache.log4j.Logger;
import connections.AthenaConnectionPool;
import connections.IAthenaConnection;

public class ConnectionTest extends BaseTest {
   private AthenaConnectionPool _pool;
   private static Logger _log = Logger.getLogger(ConnectionTest.class);
   
   public ConnectionTest() throws IOException {
      super(ConnectionTest.class);
      _pool = AthenaConnectionPool.getInstance();
      
   }
   
   protected void setUp() throws IOException {
      _log.info("setUp");
      _pool.initialize(_conf);
   }

   protected void tearDown() {
      _log.info("tearDown");
      if(_pool != null)
         _pool.closeAll();
   }
   
   public void testConnectionSetup() {
      assertEquals(_conf.getIntegerValue("ConnectionPoolSize"),
            _pool.getTotalConnections());
      _log.info("testConnectinSetup end");
   }
   
   public void testConnectionCheck()
         throws IOException, InterruptedException {
      IAthenaConnection conn = _pool.getConnection();
      assertNotNull(conn);
      if(conn != null)
         _pool.putConnection(conn);
      
      _log.info("testConnectionCheck end");
   }
}

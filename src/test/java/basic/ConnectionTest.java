package basic;

import java.io.IOException;
import connections.AthenaConnectionPool;
import connections.IAthenaConnection;

public class ConnectionTest extends BaseTest {
   private AthenaConnectionPool _pool; 
   
   public ConnectionTest() throws IOException {
      super(ConnectionTest.class);
      _pool = AthenaConnectionPool.getInstance();
      
   }
   
   protected void setUp() throws IOException {
      _pool.initialize(_conf);
   }

   protected void tearDown() {
      if(_pool != null)
         _pool.closeAll();
   }
   
   public void testConnectionSetup() {
      assertEquals(_pool.getTotalConnections(),
                   _conf.getIntegerValue("ConnectionPoolSize"));
   }
   
   public void testConnectionCheck()
         throws IOException, InterruptedException {
      IAthenaConnection conn = _pool.getConnection();
      assertNotNull(conn);
   }
}

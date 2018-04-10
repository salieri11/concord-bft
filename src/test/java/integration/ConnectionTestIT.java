package integration;

import java.io.IOException;

import base.BaseTest;
import connections.AthenaConnectionFactory;
import connections.AthenaConnectionPool;
import connections.IAthenaConnection;
import connections.AthenaConnectionFactory.ConnectionType;

public class ConnectionTestIT extends BaseTest {
   private AthenaConnectionPool _pool;

   public ConnectionTestIT() throws IOException {
      super(ConnectionTestIT.class);
      _pool = AthenaConnectionPool.getInstance();

   }

   protected void setUp() throws IOException {
      _log.info("setUp");
      _pool.initialize(_conf,
                       new AthenaConnectionFactory(ConnectionType.TCP, _conf));
   }

   protected void tearDown() {
      _log.info("tearDown");
      if (_pool != null)
         _pool.closeAll();
   }

   public void testConnectionCheck() throws IOException, InterruptedException {
      IAthenaConnection conn = _pool.getConnection();
      assertNotNull(conn);
      if (conn != null)
         _pool.putConnection(conn);

      _log.info("testConnectionCheck end");
   }

}

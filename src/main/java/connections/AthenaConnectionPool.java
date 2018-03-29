package connections;

import java.io.IOException;
import java.util.concurrent.ConcurrentLinkedQueue;
import java.util.concurrent.atomic.AtomicBoolean;
import java.util.concurrent.atomic.AtomicInteger;

import org.apache.log4j.Logger;
import configurations.IConfiguration;
import com.vmware.athena.*;
import Servlets.AthenaHelper;

public class AthenaConnectionPool {
   private AtomicInteger _connectionCount;
   private ConcurrentLinkedQueue<AthenaTCPConnection> _pool;
   private IConfiguration _conf;
   private AtomicBoolean _initialized;

   private static AthenaConnectionPool _instance =
         new AthenaConnectionPool();
   private static Logger _log =
         Logger.getLogger(AthenaConnectionPool.class);
   private static Athena.ProtocolRequest _protocolRequestMsg =
         Athena.ProtocolRequest.newBuilder()
         .setClientVersion(1)
         .build();
   private static Athena.AthenaRequest _athenaRequest =
         Athena.AthenaRequest.newBuilder()
         .setProtocolRequest(_protocolRequestMsg)
         .build();

   private AthenaConnectionPool() {
      _pool = new ConcurrentLinkedQueue<AthenaTCPConnection>();
      _initialized = new AtomicBoolean(false);
      _connectionCount = new AtomicInteger(0);
   }

   private boolean checkConnection(AthenaTCPConnection conn) {
      try {
         _log.trace("checkConnection enter");
         boolean res = AthenaHelper
               .sendToAthena(_athenaRequest, conn, _conf);
         if(res) {
            Athena.AthenaResponse resp = 
                  AthenaHelper.receiveFromAthena(conn);
            if(resp != null) {
               Athena.ProtocolResponse pResp = resp.getProtocolResponse();
               if(pResp != null) {
                  _log.debug("checkConnection, got server version: " +
                              pResp.getServerVersion());
                  return true;
               }
            }
         }
         
         return false;
      } catch (IOException e) {
         _log.error("checkConnection", e);
         return false;
      }
   }
   
   public static AthenaConnectionPool getInstance() {
      return _instance;
   }

   public IAthenaConnection getConnection()
         throws IOException, IllegalStateException {
      if (!_initialized.get())
         throw new IllegalStateException(
               "getConnection, pool not initialized");

      AthenaTCPConnection conn = _pool.poll();
      // check connection
      // need to have keep alive activity in AthenaTcpConnection
      if (conn == null) {
         conn = new AthenaTCPConnection(_conf);
         _log.info("New pooled connection created");
      }
      boolean res = checkConnection(conn);
      if (!res) {
         conn.close();
         _log.info("Broken connection closed");
         conn = new AthenaTCPConnection(_conf);
         _log.info("New pooled connection created");
      }
      
      return conn;
   }

   public void putConnection(IAthenaConnection conn)
         throws IllegalStateException, NullPointerException {
      if (!_initialized.get())
         throw new IllegalStateException(
               "returnConnection, pool not initialized");
      if (conn == null)
         throw new NullPointerException(
               "trying to return null connection");
         
      _pool.add((AthenaTCPConnection) conn);
      _connectionCount.incrementAndGet();
   }

   public void initialize(IConfiguration conf) throws IOException {
      if (_initialized.compareAndSet(false, true)) {
         _conf = conf;
         int poolSize = _conf.getIntegerValue("ConnectionPoolSize");
         for (int i = 0; i < poolSize; i++)
            putConnection(new AthenaTCPConnection(_conf));

         _log.info(String.format(
               "AthenaConnectionPool initialized with %d connections",
               poolSize));
      }
   }
   
   public void closeAll() {
      _initialized.set(false);
      for(AthenaTCPConnection conn : _pool)
         conn.close();
       
      _pool.clear();
   }

   public int getTotalConnections() {
      if (!_initialized.get())
         throw new IllegalStateException(
               "returnConnection, pool not initialized");
      return _connectionCount.get();
   }
}
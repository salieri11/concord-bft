package connections;

import java.io.IOException;
import java.util.concurrent.ArrayBlockingQueue;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.atomic.AtomicBoolean;
import java.util.concurrent.atomic.AtomicInteger;

import org.apache.log4j.Logger;
import configurations.IConfiguration;
import com.vmware.athena.*;
import Servlets.AthenaHelper;

public class AthenaConnectionPool {
   private AtomicInteger _connectionCount;
   
   // initialized with fairness = true, longest waiting threads
   // are served first
   private ArrayBlockingQueue<AthenaTCPConnection> _pool;
   
   private IConfiguration _conf;
   private AtomicBoolean _initialized;
   
   // max wait time for pool to return connection
   private int _waitTimeout;
   
   // pool can grow up to this size. currently no cleaning routine is
   // implemented, TODO
   private int _maxPoolSize;
   
   // explicit lock for the pool increase cases. to make the add operation
   // real thread safe
   private Object _poolIncreaseLock;
   
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
      _initialized = new AtomicBoolean(false);
      _connectionCount = new AtomicInteger(0);
      _poolIncreaseLock= new Object();
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
      } finally {
         _log.trace("checkConnection exit");
      }
   }
   
   public static AthenaConnectionPool getInstance() {
      return _instance;
   }

   public IAthenaConnection getConnection()
         throws IOException, IllegalStateException, InterruptedException {
      _log.trace("getConnection enter");
      
      if (!_initialized.get())
         throw new IllegalStateException(
               "getConnection, pool not initialized");

      AthenaTCPConnection conn =
            _pool.poll(_waitTimeout, TimeUnit.MILLISECONDS);
     
      if (conn == null) {
         synchronized(_poolIncreaseLock) {
            if(_connectionCount.get() < _maxPoolSize) {
               conn = new AthenaTCPConnection(_conf);
               _log.info("New pooled connection created");
            } else {
               _log.error("New pooled connection created");
               return null;
            }
         }
      }
      
      // check connection
      // need to have keep alive activity in AthenaTcpConnection
      boolean res = checkConnection(conn);
      if (!res) {
         conn.close();
         _log.info("Broken connection closed");
         conn = new AthenaTCPConnection(_conf);
         _log.info("New pooled connection created");
      }
      
      _log.trace("getConnection exit");
      
      return conn;
   }

   public void putConnection(IAthenaConnection conn)
         throws IllegalStateException, NullPointerException {
      _log.trace("putConnection enter");
      
      if (!_initialized.get())
         throw new IllegalStateException(
               "returnConnection, pool not initialized");
      if (conn == null)
         throw new NullPointerException(
               "trying to return null connection");
         
      boolean res = _pool.offer((AthenaTCPConnection) conn);
      if(res)
         _connectionCount.incrementAndGet();
      else
         _log.error("putConnection, pool is full");
      
      _log.trace("putConnection exit");
   }

   public void initialize(IConfiguration conf) throws IOException {
      if (_initialized.compareAndSet(false, true)) {
         _conf = conf;
         _waitTimeout =
               conf.getIntegerValue("ConnectionPoolWaitTimeoutMs");
         int poolSize = _conf.getIntegerValue("ConnectionPoolSize");
         int poolFactor = conf.getIntegerValue("ConnectionPoolFactor");
         _maxPoolSize = poolSize * poolFactor;
         
         _pool = 
               new ArrayBlockingQueue<AthenaTCPConnection>(_maxPoolSize,
                                                           true);
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
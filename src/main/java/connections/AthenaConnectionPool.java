/**
 * This singleton class contains methods to implement connection pooling for Helen.
 * The timeout and pool size can be adjusted from the config.properties file.
 */
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

   // Instantiate the instance of this class
   private static AthenaConnectionPool _instance = new AthenaConnectionPool();

   private static Logger _log = Logger.getLogger(AthenaConnectionPool.class);
   private static Athena.ProtocolRequest _protocolRequestMsg = Athena.ProtocolRequest
            .newBuilder().setClientVersion(1).build();
   private static Athena.AthenaRequest _athenaRequest = Athena.AthenaRequest
            .newBuilder().setProtocolRequest(_protocolRequestMsg).build();

   /**
    * Initializes local variables.
    */
   private AthenaConnectionPool() {
      _initialized = new AtomicBoolean(false);
      _connectionCount = new AtomicInteger(0);
      _poolIncreaseLock = new Object();
   }

   /**
    * Creates a new TCP connection with Athena.
    * 
    * @return
    */
   private AthenaTCPConnection createConnection() {
      _log.trace("createConnection enter");
      try {
         AthenaTCPConnection res = new AthenaTCPConnection(_conf);
         int c = _connectionCount.incrementAndGet();
         _log.debug("new connection created, active connections: " + c);
         _log.info("new pooled connection created");
         return res;
      } catch (IOException e) {
         _log.error("createConnection", e);
         return null;
      } finally {
         _log.trace("createConnection exit");
      }
   }

   /**
    * Closes a single connection instance with Athena.
    * 
    * @param conn
    */
   private void closeConnection(AthenaTCPConnection conn) {
      _log.trace("closeConnection enter");
      try {
         if (conn != null) {
            conn.close();
            int c = _connectionCount.decrementAndGet();
            _log.debug("connection closed, active connections: " + c);
            _log.info("broken connection closed");
         }
      } catch (Exception e) {
         _log.error("closeConnection", e);
      } finally {
         _log.trace("closeConnection exit");
      }
   }

   /**
    * Tests a connection by sending a protocol request to Athena.
    * 
    * @param conn
    * @return
    */
   private boolean checkConnection(AthenaTCPConnection conn) {
      try {
         _log.trace("checkConnection enter");
         boolean res = AthenaHelper.sendToAthena(_athenaRequest, conn, _conf);
         if (res) {
            Athena.AthenaResponse resp = AthenaHelper.receiveFromAthena(conn);
            if (resp != null) {
               Athena.ProtocolResponse pResp = resp.getProtocolResponse();
               if (pResp != null) {
                  _log.debug("checkConnection, got server version: "
                           + pResp.getServerVersion());
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

   /**
    * Returns the single instance of this class.
    * 
    * @return
    */
   public static AthenaConnectionPool getInstance() {
      return _instance;
   }

   /**
    * Removes a connection from the connection pool data structure, checks it,
    * and returns it.
    * 
    * @return
    * @throws IOException
    * @throws IllegalStateException
    * @throws InterruptedException
    */
   // TODO If checkConnection fails, shouldn't we return another connection?
   public IAthenaConnection getConnection()
            throws IOException, IllegalStateException, InterruptedException {
      _log.trace("getConnection enter");

      if (!_initialized.get())
         throw new IllegalStateException("getConnection, pool not initialized");

      AthenaTCPConnection conn = _pool.poll(_waitTimeout,
               TimeUnit.MILLISECONDS);

      if (conn == null) {
         synchronized (_poolIncreaseLock) {
            if (_connectionCount.get() < _maxPoolSize) {
               conn = createConnection();
            } else {
               _log.error("pool size at maximum");
               return null;
            }
         }
      }

      // check connection
      boolean res = checkConnection(conn);
      if (!res) {
         _log.error("");
         closeConnection(conn);
         return null;
      }

      _log.trace("getConnection exit");

      return conn;
   }

   /**
    * Adds a connection to the connection pool data structure.
    * 
    * @param conn
    * @throws IllegalStateException
    * @throws NullPointerException
    */
   public void putConnection(IAthenaConnection conn)
            throws IllegalStateException, NullPointerException {
      _log.trace("putConnection enter");

      if (!_initialized.get())
         throw new IllegalStateException(
                  "returnConnection, pool not initialized");

      // cannot be null in normal flow
      if (conn == null) {
         _log.fatal("putConnection, conn is null");
      } else {
         boolean res = _pool.offer((AthenaTCPConnection) conn);

         // cannot fail in normal flow
         if (!res) {
            ((AthenaTCPConnection) conn).close();
            _log.fatal("putConnection, pool at maximum");
         }
      }

      _log.trace("putConnection exit");
   }

   /**
    * Reads connection pool related configurations.
    * 
    * @param conf
    * @throws IOException
    */
   public void initialize(IConfiguration conf) throws IOException {
      if (_initialized.compareAndSet(false, true)) {
         _conf = conf;
         _waitTimeout = conf.getIntegerValue("ConnectionPoolWaitTimeoutMs");
         int poolSize = _conf.getIntegerValue("ConnectionPoolSize");
         int poolFactor = conf.getIntegerValue("ConnectionPoolFactor");
         _maxPoolSize = poolSize * poolFactor;

         _pool = new ArrayBlockingQueue<AthenaTCPConnection>(_maxPoolSize,
                  true);
         for (int i = 0; i < poolSize; i++)
            putConnection(createConnection());

         _log.info(String.format(
                  "AthenaConnectionPool initialized with %d connections",
                  _connectionCount.get()));
      }
   }

   /**
    * Closes all connections in the connection pool
    */
   public void closeAll() {
      _initialized.set(false);
      for (AthenaTCPConnection conn : _pool) {
         closeConnection(conn);
      }

      _pool.clear();
      _connectionCount.set(0);
   }

   /**
    * Returns total number of connections
    * 
    * @return
    */
   public int getTotalConnections() {
      if (!_initialized.get())
         throw new IllegalStateException(
                  "returnConnection, pool not initialized");
      return _connectionCount.get();
   }
}
/**
 * This singleton class contains methods to implement connection pooling for
 * Helen. The timeout and pool size can be adjusted from the
 * application.properties file.
 */
package connections;

import java.io.IOException;
import java.util.concurrent.ArrayBlockingQueue;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.atomic.AtomicBoolean;
import java.util.concurrent.atomic.AtomicInteger;

import org.apache.log4j.Logger;

import configurations.IConfiguration;

public class AthenaConnectionPool {
   // Instantiate the instance of this class
   private static AthenaConnectionPool _instance = new AthenaConnectionPool();
   private static Logger _log = Logger.getLogger(AthenaConnectionPool.class);
   private AtomicInteger _connectionCount;
   // initialized with fairness = true, longest waiting threads
   // are served first
   private ArrayBlockingQueue<IAthenaConnection> _pool;
   private IConfiguration _conf;
   private AtomicBoolean _initialized;
   // max wait time for pool to return connection
   private int _waitTimeout;
   // pool starts at this size
   private int _minPoolSize;
   // pool can grow up to this size. currently no cleaning routine is
   // implemented, TODO
   private int _maxPoolSize;
   private AthenaConnectionFactory _factory;

   /**
    * Initializes local variables.
    */
   private AthenaConnectionPool() {
      _initialized = new AtomicBoolean(false);
      _connectionCount = new AtomicInteger(0);
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
    * Creates a new TCP connection with Athena.
    *
    * @return
    */
   private IAthenaConnection createConnection() {
      _log.trace("createConnection enter");
      try {
         // increment first, so that all errors can decrement
         int c = _connectionCount.incrementAndGet();
         if (c <= _maxPoolSize) {
            IAthenaConnection res = _factory.create();
            _log.info("new connection created, active connections: " + c);
            return res;
         } else {
            _log.debug("pool size at maximum");
            _connectionCount.decrementAndGet();
            return null;
         }
      } catch (Exception e) {
         // all exceptions are failures - undo the increment, since we failed
         _connectionCount.decrementAndGet();
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
   private void closeConnection(IAthenaConnection conn) {
      _log.trace("closeConnection enter");
      try {
         if (conn != null) {
            conn.close();
            int c = _connectionCount.decrementAndGet();
            _log.debug("connection closed, active connections: " + c);
            _log.info("broken connection closed");

            // attempt to replace the broken connection
            if (c < _minPoolSize && _initialized.get()) {
               putConnection(createConnection());
            }
         }
      } catch (Exception e) {
         _log.error("closeConnection", e);
      } finally {
         _log.trace("closeConnection exit");
      }
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
   public IAthenaConnection getConnection() throws IOException,
                                            IllegalStateException,
                                            InterruptedException {
      _log.trace("getConnection enter");

      if (!_initialized.get())
         throw new IllegalStateException("getConnection, pool not initialized");

      boolean first = true;
      long start = System.currentTimeMillis();
      while (System.currentTimeMillis() - start < _waitTimeout) {
         IAthenaConnection conn;

         if (first) {
            // don't wait on the first poll; if the pool is empty, jump
            // immediately to checking if a new connection can be added
            first = false;
            conn = _pool.poll();

            if (conn == null) {
               // this may fail if there are _maxPoolSize connections already
               conn = createConnection();
            }
         } else {
            // if this is not our first wait, then we weren't allowed to
            // increase the pool size, so we just have to wait for a connection
            conn = _pool.poll(_waitTimeout, TimeUnit.MILLISECONDS);
         }

         if (conn != null) {
            boolean res = conn.check();
            if (!res) {
               _log.error("Failed to check connection");
               closeConnection(conn);
               // see if we can get another connection
               continue;
            }

            _log.trace("getConnection exit");
            return conn;
         }
      }

      _log.trace("getConnection exit");
      return null;
   }

   /**
    * Adds a connection to the connection pool data structure.
    *
    * @param conn
    * @throws IllegalStateException
    * @throws NullPointerException
    */
   public void
          putConnection(IAthenaConnection conn) throws IllegalStateException,
                                                NullPointerException {
      _log.trace("putConnection enter");

      if (!_initialized.get())
         throw new IllegalStateException("returnConnection, pool not initialized");

      // cannot be null in normal flow
      if (conn == null) {
         _log.fatal("putConnection, conn is null");
      } else {
         boolean res = _pool.offer(conn);

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
   public void initialize(IConfiguration conf,
                          AthenaConnectionFactory factory) throws IOException {
      if (_initialized.compareAndSet(false, true)) {
         _conf = conf;
         _factory = factory;
         _waitTimeout = conf.getIntegerValue("ConnectionPoolWaitTimeoutMs");
         _minPoolSize = _conf.getIntegerValue("ConnectionPoolSize");
         int poolFactor = conf.getIntegerValue("ConnectionPoolFactor");
         _maxPoolSize = _minPoolSize * poolFactor;

         _pool = new ArrayBlockingQueue<IAthenaConnection>(_maxPoolSize, true);
         for (int i = 0; i < _minPoolSize; i++) {
            putConnection(createConnection());
         }

         _log.info(String.format("AthenaConnectionPool initialized with %d connections",
                                 _connectionCount.get()));
      }
   }

   /**
    * Closes all connections in the connection pool
    */
   public void closeAll() {
      _initialized.set(false);
      for (IAthenaConnection conn : _pool) {
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
      if (!_initialized.get()) {
         throw new IllegalStateException("returnConnection, pool not initialized");
      }
      return _connectionCount.get();
   }
}

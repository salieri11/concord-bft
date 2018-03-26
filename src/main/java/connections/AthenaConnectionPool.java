package connections;

import java.io.IOException;
import java.util.concurrent.ConcurrentLinkedQueue;
import java.util.concurrent.atomic.AtomicBoolean;
import org.apache.log4j.Logger;
import configurations.IConfiguration;
import connections.*;

public class AthenaConnectionPool {
	private ConcurrentLinkedQueue<AthenaTCPConnection> _pool;
	private IConfiguration _conf;
	private AtomicBoolean _initialized;
	
	private static AthenaConnectionPool _instance = new AthenaConnectionPool();
	private static Logger _log = Logger.getLogger(AthenaConnectionPool.class);
			
	private AthenaConnectionPool() {
		_pool = new ConcurrentLinkedQueue<AthenaTCPConnection>();
		_initialized = new AtomicBoolean(false);
	}
		
	public static AthenaConnectionPool getInstance() {
		return _instance;
	}
	
	public IAthenaConnection getConnection() throws IOException, IllegalStateException {
		if(!_initialized.get())
			throw new IllegalStateException("getConnection, pool not initialized");
		
		AthenaTCPConnection conn = _pool.poll();
		//check connection
		// need to have keep alive activity in AthenaTcpConnection
		if(conn == null) {
			conn = new AthenaTCPConnection(_conf);
			_log.warn("New pooled connection created");
		}
		if (!conn.check()) {
			conn.close();
			_log.warn("Broken connection closed");
			conn = new AthenaTCPConnection(_conf);
			_log.warn("New pooled connection created");
		}
		
		return conn;
	}
	
	public void putConnection(IAthenaConnection conn) throws IllegalStateException {
		if(!_initialized.get())
			throw new IllegalStateException("returnConnection, pool not initialized");
		
		_pool.add((AthenaTCPConnection)conn);
	}
	
	public void initialize(IConfiguration conf) throws IOException {
		if(_initialized.compareAndSet(false, true)) {
			_conf = conf;
			int poolSize = _conf.getIntegerValue("ConnectionPoolSize");
			for(int i = 0; i < poolSize; i++)
				_pool.add(new AthenaTCPConnection(_conf));
			
			_log.info(String.format("AthenaConnectionPool initialized with %d connections", poolSize));
		}
	}
}
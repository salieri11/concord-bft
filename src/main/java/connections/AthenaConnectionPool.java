import java.util.concurrent.ConcurrentSkipListSet;

import org.apache.log4j.Logger;

import configurations.IConfiguration;

package connections;

public class AthenaConnectionPool {
	private ConcurrentSkipListSet<IAthenaConnection> _pool;
	private IConfiguration _conf;	
	private static AthenaConnectionPool _instance = new AthenaConnectionPool();
	private static Logger _log;
	
	public static AthenaConnectionPool getInstance() {
		return _instance;
	}
	
	public IAthenaConnection getConnection() {
		IAthenaConnection conn = _pool.first();
		if(conn == null) {
			_log.warn("New pooled connection created");
			createConnection();
		}
	}
	
	public void initialize(IConfiguration conf) {
		
	}
	
	private void ccreateConnection() {
		AthenaTCPConnection conn = new AthenaTCPConnection(_conf);
	}
}
 

package connections;

import java.io.IOException;

import configurations.IConfiguration;

public final class AthenaConnectionFactory {
   private ConnectionType _type;
   private IConfiguration _conf;

   public enum ConnectionType {
      TCP,
      Mock
   }

   public AthenaConnectionFactory(ConnectionType type, IConfiguration conf) {
      _type = type;
      _conf = conf;
   }

   public IAthenaConnection create() throws IOException,
                                     UnsupportedOperationException {
      switch (_type) {
      case TCP:
         return new AthenaTCPConnection(_conf);
      case Mock:
         return new MockConnection(_conf);
      }

      return null;
   }
}

package connections;

import java.io.IOException;
import java.util.ArrayList;
import java.util.Random;
import java.util.concurrent.atomic.AtomicLong;

import configurations.IConfiguration;

public final class AthenaConnectionFactory {
   private ConnectionType _type;
   private IConfiguration _conf;
   private ArrayList<Authority> athenaList;
   private AtomicLong nextAuthority;

   public enum ConnectionType {
      TCP,
      Mock
   }

   public AthenaConnectionFactory(ConnectionType type, IConfiguration conf) {
      _type = type;
      _conf = conf;
      nextAuthority = new AtomicLong();

      //Read list of athenas from config
      athenaList = new ArrayList<>();
      String authorities = _conf.getStringValue("AthenaAuthorities");
      String[] authorityList = authorities.split(",");
      for (String authority : authorityList) {
         String[] group = authority.split(":");
         Authority a = new Authority(group[0], Integer.parseInt(group[1]));
         athenaList.add(a);
      }
   }

   public IAthenaConnection create() throws IOException,
                                     UnsupportedOperationException {
      switch (_type) {
      case TCP:
         //Select an Athena instance to connect with in a round robin fashion
         int chosenAuthority 
            = (int)nextAuthority.getAndIncrement() % athenaList.size();
         Authority athenaInstance = athenaList.get(chosenAuthority);
         AthenaTCPConnection connection
            = new AthenaTCPConnection(_conf,
                                      athenaInstance.getHost(),
                                      athenaInstance.getPort());
         return connection;
      case Mock:
         return new MockConnection(_conf);
      default:
         throw new UnsupportedOperationException("type not supported" + _type);
      }
   }

   private class Authority {
      String host;
      int port;

      Authority(String h, int p) {
         host = h;
         port = p;
      }

      String getHost() {
         return host;
      }

      int getPort() {
         return port;
      }
   }
}

/**
 * Main class for helen, does some basic initializations and then calls
 * SpringApplication.run() method. This class also does the job of providing all
 * spring related configuration annotations
 *
 * Helen connects to Athena at the backend. Communication between Helen and
 * Athena is via a TCP socket connection. Messages are sent in Google Protocol
 * Buffer format. Responses from Helen to the client are in Json format.
 *
 */
package Servers;

import java.io.IOException;
import java.text.SimpleDateFormat;
import java.util.Date;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.springframework.boot.SpringApplication;
import org.springframework.boot.autoconfigure.SpringBootApplication;
import org.springframework.boot.autoconfigure.domain.EntityScan;
import org.springframework.context.annotation.ComponentScan;
import org.springframework.data.jpa.repository.config.EnableJpaRepositories;

import Servlets.BlockList;
import configurations.ConfigurationFactory;
import configurations.ConfigurationFactory.ConfigurationType;
import configurations.IConfiguration;
import connections.AthenaConnectionFactory;
import connections.AthenaConnectionPool;
import services.profiles.ProfilesRegistryManager;
import services.profiles.User;

@SpringBootApplication
@EntityScan(basePackageClasses = { User.class })
@EnableJpaRepositories(basePackageClasses = { ProfilesRegistryManager.class })
@ComponentScan(basePackageClasses = { BlockList.class,
   ProfilesRegistryManager.class, HelenSpringWebConfig.class })
public class Server {

   // Set current datetime for logging purposes
   static {
      SimpleDateFormat dateFormat = new SimpleDateFormat("dd-MM-yyyy-hh-mm-ss");
      System.setProperty("current.date.time", dateFormat.format(new Date()));
   }

   public static void main(String[] args) throws IOException {
      final Logger logger = LogManager.getLogger(Server.class);

      if (args.length == 1) {
         // This accepts only 1 argument and it is name of configuration file
         ConfigurationFactory.init(args[0]);
      } else {
         ConfigurationFactory.init();
      }

      // Read configurations file
      IConfiguration conf
         = ConfigurationFactory.getConfiguration(ConfigurationType.File);

      AthenaConnectionFactory factory
         = new AthenaConnectionFactory(AthenaConnectionFactory.ConnectionType.TCP,
                                       conf);
      AthenaConnectionPool.getInstance().initialize(conf, factory);
      logger.info("athena connection pool initialized");

      SpringApplication.run(Server.class, args);
   }

}

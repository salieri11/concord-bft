/**
 * Copyright 2018 VMware, all rights reserved.
 *
 */

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
import java.util.concurrent.TimeUnit;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.springframework.boot.SpringApplication;
import org.springframework.boot.autoconfigure.SpringBootApplication;
import org.springframework.boot.autoconfigure.domain.EntityScan;
import org.springframework.cache.CacheManager;
import org.springframework.cache.annotation.EnableCaching;
import org.springframework.cache.ehcache.EhCacheCacheManager;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.ComponentScan;
import org.springframework.data.jpa.repository.config.EnableJpaRepositories;

import Servlets.BlockList;
import configurations.ConfigurationFactory;
import configurations.ConfigurationFactory.ConfigurationType;
import configurations.IConfiguration;
import connections.AthenaConnectionFactory;
import connections.AthenaConnectionPool;
import net.sf.ehcache.config.CacheConfiguration;
import services.profiles.ProfilesRegistryManager;
import services.profiles.User;

@SpringBootApplication
@EntityScan(basePackageClasses = { User.class })
@EnableJpaRepositories(basePackageClasses = { ProfilesRegistryManager.class })
@ComponentScan(basePackageClasses = { BlockList.class,
   ProfilesRegistryManager.class, HelenSpringWebConfig.class })
@EnableCaching
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

   private net.sf.ehcache.CacheManager ehCacheManager() {
       //TODO visit these numbers
       CacheConfiguration cacheConfiguration = new CacheConfiguration();
       cacheConfiguration.setName("TokenCache");
       cacheConfiguration.setMemoryStoreEvictionPolicy("LRU");
       cacheConfiguration.setMaxEntriesLocalHeap(500);
       cacheConfiguration.timeToIdleSeconds(TimeUnit.MINUTES.toSeconds(5));
       cacheConfiguration.timeToLiveSeconds(TimeUnit.MINUTES.toSeconds(5));

       net.sf.ehcache.config.Configuration config = new net.sf.ehcache.config.Configuration();
       config.addCache(cacheConfiguration);

       return net.sf.ehcache.CacheManager.newInstance(config);
   }

   @Bean
   CacheManager cacheManager() {
       return new EhCacheCacheManager(ehCacheManager());
   }



}

package connections;

import java.io.IOException;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;

import configurations.AthenaProperties;

/**
 * Temporary configuration to create connection pool bean.
 */
@Configuration
public class AthenaConfiguration {
    private final Logger logger = LogManager.getLogger(AthenaConfiguration.class);
    private AthenaProperties config;

    @Autowired
    public AthenaConfiguration(AthenaProperties config) {
        this.config = config;
    }

    @Bean
    AthenaConnectionPool athenaConnectionPool() throws IOException {
        AthenaConnectionFactory factory = new AthenaConnectionFactory(AthenaConnectionFactory.ConnectionType.TCP, config);
        AthenaConnectionPool pool = new AthenaConnectionPool().initialize(config, factory);
        logger.info("athena connection pool initialized");
        return pool;
    }

}

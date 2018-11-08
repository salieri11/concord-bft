/*
 * Copyright (c) 2018 VMware, Inc. All rights reserved. VMware Confidential
 */

/**
 * Main class for helen, does some basic initializations and then calls SpringApplication.run() method. This class also
 * does the job of providing all spring related configuration annotations
 *
 * Helen connects to Athena at the backend. Communication between Helen and Athena is via a TCP socket connection.
 * Messages are sent in Google Protocol Buffer format. Responses from Helen to the client are in Json format.
 *
 */
package com.vmware.blockchain;

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
import org.springframework.data.jpa.repository.config.EnableJpaRepositories;

import net.sf.ehcache.config.CacheConfiguration;

@SpringBootApplication
@EntityScan(basePackages = "com.vmware.blockchain")
@EnableJpaRepositories(basePackages = "com.vmware.blockchain")
@EnableCaching
public class Server {


    // Set current datetime for logging purposes
    static {
        SimpleDateFormat dateFormat = new SimpleDateFormat("dd-MM-yyyy-hh-mm-ss");
        System.setProperty("current.date.time", dateFormat.format(new Date()));
    }

    public static void main(String[] args) throws IOException {
        final Logger logger = LogManager.getLogger(Server.class);
        String[] testEnv = {"--spring.profiles.active=test"};

        // backwards compatibility for testing
        if (args.length == 1) {
            // used to hand in a property file name. Make the argument the active profile
            if ("application-test.properties".equals(args[0])) {
                args = testEnv;
                logger.info("Setting test environment");
            }
        }

        SpringApplication.run(Server.class, args);
    }

    private net.sf.ehcache.CacheManager ehCacheManager() {
        // TODO visit these numbers
        CacheConfiguration cacheConfiguration = new CacheConfiguration();
        cacheConfiguration.setName("UserCache");
        cacheConfiguration.setMemoryStoreEvictionPolicy("LRU");
        cacheConfiguration.setMaxEntriesLocalHeap(500);
        cacheConfiguration.timeToIdleSeconds(TimeUnit.MINUTES.toSeconds(5));
        cacheConfiguration.timeToLiveSeconds(TimeUnit.MINUTES.toSeconds(5));
        cacheConfiguration.copyOnRead(true);
        cacheConfiguration.copyOnWrite(true);

        net.sf.ehcache.config.Configuration config = new net.sf.ehcache.config.Configuration();
        config.addCache(cacheConfiguration);

        return net.sf.ehcache.CacheManager.newInstance(config);
    }

    @Bean
    CacheManager cacheManager() {
        return new EhCacheCacheManager(ehCacheManager());
    }



}

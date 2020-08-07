/*
 * Copyright (c) 2020 VMware, Inc. All rights reserved. VMware Confidential
 */

package com.vmware.blockchain;

import static org.ehcache.config.builders.CacheConfigurationBuilder.newCacheConfigurationBuilder;
import static org.ehcache.config.builders.ExpiryPolicyBuilder.timeToLiveExpiration;
import static org.ehcache.config.builders.ResourcePoolsBuilder.heap;

import javax.cache.CacheManager;
import javax.cache.Caching;
import javax.cache.spi.CachingProvider;

import org.ehcache.config.CacheConfiguration;
import org.ehcache.config.builders.ConfigurationBuilder;
import org.ehcache.jsr107.EhcacheCachingProvider;
import org.springframework.cache.annotation.EnableCaching;
import org.springframework.cache.jcache.JCacheCacheManager;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.security.core.Authentication;
import org.springframework.security.core.userdetails.UserDetails;

import com.vmware.blockchain.common.Constants;
import com.vmware.blockchain.security.HelenUserDetails;

/**
 * CacheConfig.  Configuration for Spring Caching
 */
@Configuration
@EnableCaching
public class CacheConfig {
    /**
     * Initialize the User Cache.
     * We are using ehCahche 3.x, which now supports the JSR-107 (JCache) spec.  Spring uses JCache when the
     * the javax.cache dependency is included in the POM.  When we do that, we need to also inlcude a provider,
     * so we are using org.ehache.
     * <br/>
     * Please see @link(https://www.ehcache.org/documentation/3.8/107.html) for a description of this configuration.
     */
    @Bean
    org.springframework.cache.CacheManager cacheManager() {
        // get our caching provider.  If this isn't ehCache, we will get a class mismatch here.
        CachingProvider cachingProvider = Caching.getCachingProvider();
        EhcacheCachingProvider ehcacheProvider = (EhcacheCachingProvider) cachingProvider;
        // Three caches, all pretty much the same.  Limit to 500 elements, 5 minute TTL.
        org.ehcache.config.Configuration configuration = ConfigurationBuilder.newConfigurationBuilder()
                .addCache("UserCache",
                          (CacheConfiguration<?, ?>) newCacheConfigurationBuilder(String.class, UserDetails.class,
                                                                                  heap(500))
                                  .withExpiry(timeToLiveExpiration(java.time.Duration.ofMinutes(5))).build())
                .addCache(Constants.TOKEN_CACHE,
                          (CacheConfiguration<?, ?>) newCacheConfigurationBuilder(String.class, Authentication.class,
                                                                                  heap(500))
                                  .withExpiry(timeToLiveExpiration(java.time.Duration.ofMinutes(5))).build())
                .addCache(Constants.CSP_TOKEN_CACHE,
                          (CacheConfiguration<?, ?>) newCacheConfigurationBuilder(String.class, HelenUserDetails.class,
                                                                                  heap(500))
                                  .withExpiry(timeToLiveExpiration(java.time.Duration.ofMinutes(5))).build()).build();
        // Create the javax.cache.CacheManager from the ehProvider instance
        CacheManager cacheManager = ehcacheProvider.getCacheManager(ehcacheProvider.getDefaultURI(), configuration);
        // wrap this in the Spring JCacheManager so Spring can use it
        return new JCacheCacheManager(cacheManager);
    }

}

/*
 * Copyright (c) 2019 VMware, Inc. All rights reserved. VMware Confidential
 */

package com.vmware.blockchain;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.cache.Cache;
import org.springframework.cache.CacheManager;
import org.springframework.stereotype.Component;

/**
 * After extensive testing it appears that @CacheEvict simply doesn't work.
 * So we need to explicitly invalidate.
 */
@Component
public class BaseCacheHelper {

    private CacheManager cacheManager;

    @Autowired
    public BaseCacheHelper(CacheManager cacheManager) {
        this.cacheManager = cacheManager;
    }

    /**
     * Force invalidation.
     */
    public void evict(String cacheName, Object key) {
        Cache cache = cacheManager.getCache(cacheName);
        cache.evict(key);
    }

    /**
     * Fetch value from cache.
     */
    public <T> T getCacheValue(String cacheName, Object key, Class<T> clazz) {
        Cache cache = cacheManager.getCache(cacheName);
        Cache.ValueWrapper value = cache.get(key);
        if (value != null) {
            return clazz.cast(value.get());
        }
        return null;
    }
}

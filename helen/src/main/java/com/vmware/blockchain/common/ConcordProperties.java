/*
 * Copyright (c) 2018-2019 VMware, Inc. All rights reserved. VMware Confidential
 */

package com.vmware.blockchain.common;

import java.util.HashMap;
import java.util.Map;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import org.springframework.beans.BeanUtils;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Component;

import com.google.common.base.Splitter;

import lombok.Getter;
import lombok.Setter;

/**
 * Propeties per Concord Blockchain.  Will need to consider the connection pool.
 */
@Component
@Getter
@Setter
public class ConcordProperties {

    // Concord configurations
    @Value("${ConcordAuthorities}")
    String concordAuthorities;
    @Value("${ConnectionPoolSize}")
    int connectionPoolSize;
    @Value("${ConnectionPoolFactor}")
    int connectionPoolFactor;
    @Value("${ConnectionPoolWaitTimeoutMs}")
    int connectionPoolWaitTimeoutMs;
    @Value("${ReceiveTimeoutMs}")
    int receiveTimeoutMs;
    @Value("${ReceiveHeaderSizeBytes}")
    int receiveHeaderSizeBytes;
    @Value("${ConcordRpcUrls}")
    String concordRpcUrls;
    // parsed concordRpcUrls
    Map<String, String> concordRpcUrlsMap;
    @Value("${ConcordRpcCerts}")
    String concordRpcCerts;
    // parsed concordRpcCerts
    Map<String, String> concordRpcCertsMap;

    /**
     * Create a copy of the default ConcordProperties.
     * @return Shallow copy
     */
    public ConcordProperties instance() {
        ConcordProperties prop = new ConcordProperties();
        BeanUtils.copyProperties(this, prop);
        return prop;
    }

    /**
     * ConcordRpcUrls should be of the format:
     *     hostname1=url1,hostname2=url2,...
     * This function splits such a string into a map of hostname to url.
     */
    public Map<String, String> getRpcUrlsAsMap() {
        if (concordRpcUrlsMap == null) {
            try {
                concordRpcUrlsMap = Splitter.on(",").withKeyValueSeparator("=").split(concordRpcUrls);
            } catch (IllegalArgumentException e) {
                // swallow this exception: if the format was invalid, just don't expose URLs
                Logger log = LoggerFactory.getLogger(ConcordProperties.class);
                log.warn("Unable to parse concordRpcUrls", e);

                // also set concordRpcUrlsMap so we don't waste cycles re-evaluating this string
                concordRpcUrlsMap = new HashMap<String, String>();
            }
        }
        return concordRpcUrlsMap;
    }

    /**
     * ConcordRpcCerts should be of the format:
     *     hostname1=url1,hostname2=url2,...
     * This function splits such a string into a map of hostname to url.
     */
    public Map<String, String> getRpcCertsAsMap() {
        if (concordRpcCertsMap == null) {
            try {
                concordRpcCertsMap = Splitter.on(",").withKeyValueSeparator("=").split(concordRpcCerts);
            } catch (IllegalArgumentException e) {
                // swallow this exception: if the format was invalid, just don't expose URLs
                Logger log = LoggerFactory.getLogger(ConcordProperties.class);
                log.warn("Unable to parse concordRpcCerts", e);

                // also set concordRpcCertsMap so we don't waste cycles re-evaluating this string
                concordRpcCertsMap = new HashMap<String, String>();
            }
        }
        return concordRpcCertsMap;
    }
}

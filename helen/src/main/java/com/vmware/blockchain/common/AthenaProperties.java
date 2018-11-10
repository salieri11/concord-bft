/*
 * Copyright (c) 2018 VMware, Inc. All rights reserved. VMware Confidential
 */

package com.vmware.blockchain.common;

import org.springframework.beans.BeanUtils;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Component;

import lombok.Getter;
import lombok.Setter;

/**
 * Propeties per Athena Blockchain.  Will need to consider the connection pool.
 */
@Component
@Getter
@Setter
public class AthenaProperties {

    // Athena configurations
    @Value("${AthenaAuthorities}")
    String athenaAuthorities;
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


    /**
     * Create a copy of the default AthenaProperties.
     * @return Shallow copy
     */
    public AthenaProperties instance() {
        AthenaProperties prop = new AthenaProperties();
        BeanUtils.copyProperties(this, prop);
        return prop;
    }
}

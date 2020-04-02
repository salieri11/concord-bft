/*
 * Copyright (c) 2020 VMware, Inc. All rights reserved. VMware Confidential
 */

package com.vmware.blockchain.deployment.services.restclient;

import com.fasterxml.jackson.databind.DeserializationFeature;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.fasterxml.jackson.databind.PropertyNamingStrategy;

/**
 * Helper class for Rest API calls.
 */
public final class RestClientUtils {

    /**
     * Default Object Mapper.
     */
    public static ObjectMapper getDefaultMapper() {
        ObjectMapper om = new ObjectMapper();
        om.configure(DeserializationFeature.FAIL_ON_UNKNOWN_PROPERTIES, false);
        om.setPropertyNamingStrategy(PropertyNamingStrategy.SNAKE_CASE);

        return om;
    }
}

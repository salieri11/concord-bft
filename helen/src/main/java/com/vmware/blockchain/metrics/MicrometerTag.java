/*
 * Copyright (c) 2019 VMware, Inc. All rights reserved. VMware Confidential
 */

package com.vmware.blockchain.metrics;

import io.micrometer.core.instrument.Tag;

/**
 * Wrapper for micrometer tag interface to handle null tag values.
 */
public interface MicrometerTag {

    static Tag of(String key, String value) {
        return Tag.of(key, value != null ? value : "unknown");
    }

    static Tag of(String key, int value) {
        return Tag.of(key, String.valueOf(value));
    }

}
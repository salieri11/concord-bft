/*
 * Copyright (c) 2020 VMware, Inc. All rights reserved. VMware Confidential
 */

package com.vmware.blockchain.deployment.services.orchestration.model.vsphere;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;

/**
 * Data class.
 */
@Data
@Builder
@AllArgsConstructor
public class StorageMapping {
    String key;
    StorageGroupMapping value;

    /**
     * test.
     */
    @Builder
    @Data
    @AllArgsConstructor
    public static class StorageGroupMapping {
        String type;
        String datastoreId;
    }
}


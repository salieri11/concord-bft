/*
 * Copyright (c) 2020 VMware, Inc. All rights reserved. VMware Confidential
 */

package com.vmware.blockchain.deployment.services.orchestration.model.vsphere;

import com.fasterxml.jackson.annotation.JsonInclude;
import com.fasterxml.jackson.annotation.JsonProperty;

import lombok.Builder;
import lombok.Data;

/**
 * Data class.
 */
@Data
public class VirtualMachineUpdate {

    /**
     * Data class.
     */
    @Data
    @Builder
    public static class VirtualMachineUpdateRequest {
        VirtualMachineUpdateSpec spec;
    }

    /**
     * Data class.
     */
    @JsonInclude(JsonInclude.Include.NON_DEFAULT)
    @Data
    @Builder
    public static class VirtualMachineUpdateSpec {
        @JsonProperty("hot_add_enabled")
        boolean hotAddEnabled;

        @JsonProperty("hot_remove_enabled")
        boolean hotRemoveEnabled;

        @JsonProperty("size_MiB")
        long sizeMiB;

        @JsonProperty("cores_per_socket")
        int coresPerSocket;

        int count;
    }

}

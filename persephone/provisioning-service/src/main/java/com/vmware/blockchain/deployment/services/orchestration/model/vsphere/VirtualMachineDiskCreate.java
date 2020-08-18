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
public class VirtualMachineDiskCreate {

    /**
     * Data class.
     */
    @Data
    @Builder
    public static class VirtualMachineDiskCreateRequest {
        DiskCreateSpec spec;
    }

    /**
     * Data class.
     */
    @JsonInclude(JsonInclude.Include.NON_DEFAULT)
    @Data
    @Builder
    public static class DiskCreateSpec {
        @JsonProperty("new_vmdk")
        DiskVmdkCreateSpec newVmdk;

        @JsonProperty("type")
        String type;
    }

    /**
     * Data class.
     */
    @Data
    @Builder
    public static class DiskVmdkCreateSpec {
        @JsonProperty("capacity")
        long capacity;

        @JsonProperty("name")
        String name;

        @JsonProperty("storage_policy")
        DiskStoragePolicySpec storagePolicy;
    }

    /**
     * Enum.
     */
    public enum DiskHostBusAdapterType {
        IDE,
        NVME,
        SATA,
        SCSI;
    }

    /**
     * Data class.
     */
    @Data
    @Builder
    public static class DiskStoragePolicySpec {
        @JsonProperty("policy")
        String policy;
    }

}

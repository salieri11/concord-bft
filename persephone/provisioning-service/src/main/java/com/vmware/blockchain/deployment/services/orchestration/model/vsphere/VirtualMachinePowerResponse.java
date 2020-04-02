/*
 * Copyright (c) 2020 VMware, Inc. All rights reserved. VMware Confidential
 */

package com.vmware.blockchain.deployment.services.orchestration.model.vsphere;

import lombok.Data;
import lombok.NoArgsConstructor;

/**
 * Data class.
 */
@Data
@NoArgsConstructor
public class VirtualMachinePowerResponse {
    VirtualMachinePowerInfo value;

    /**
     * Data class.
     */
    @Data
    @NoArgsConstructor
    public static class VirtualMachinePowerInfo {
        VirtualMachinePowerState state;
    }
}

/*
 * Copyright (c) 2020 VMware, Inc. All rights reserved. VMware Confidential
 */

package com.vmware.blockchain.deployment.services.orchestration.model.vsphere;

import lombok.Data;

/**
 * Data class for Health response.
 */
@Data
public class GetHealthResponse {
    public static String HEALTHY = "green";

    String value;
}

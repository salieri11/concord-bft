/*
 * Copyright (c) 2020 VMware, Inc. All rights reserved. VMware Confidential
 */

package com.vmware.blockchain.deployment.services.orchestration.model.vsphere;

import lombok.Data;

/**
 * Data class.
 */
@Data
public class NetworkSummary {
    String network;
    String name;
    String type;
}

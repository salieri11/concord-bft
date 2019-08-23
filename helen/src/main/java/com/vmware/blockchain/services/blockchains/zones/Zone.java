/*
 * Copyright (c) 2019 VMware, Inc. All rights reserved. VMware Confidential
 */

package com.vmware.blockchain.services.blockchains.zones;

import java.util.Map;
import java.util.UUID;

import com.vmware.blockchain.deployment.v1.OrchestrationSiteInfo;

import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;

/**
 * The Orchestration Site view from Persephone.
 */
@Data
@AllArgsConstructor
@NoArgsConstructor
public class Zone {

    private UUID id;
    OrchestrationSiteInfo.Type type;
    Map<String, String> labels;
}

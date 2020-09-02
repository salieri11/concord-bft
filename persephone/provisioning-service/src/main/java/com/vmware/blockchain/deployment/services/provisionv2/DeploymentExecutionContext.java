/* **************************************************************************
 * Copyright (c) 2019 VMware, Inc. All rights reserved. VMware Confidential
 * **************************************************************************/

package com.vmware.blockchain.deployment.services.provisionv2;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.UUID;
import java.util.concurrent.ConcurrentHashMap;

import com.vmware.blockchain.deployment.services.orchestration.Orchestrator;
import com.vmware.blockchain.deployment.v1.BlockchainType;
import com.vmware.blockchain.deployment.v1.ConcordComponent;
import com.vmware.blockchain.deployment.v1.DeployedResource;
import com.vmware.blockchain.deployment.v1.DeploymentExecutionEvent;
import com.vmware.blockchain.deployment.v1.NodeAssignment;
import com.vmware.blockchain.deployment.v1.OrchestrationSiteIdentifier;
import com.vmware.blockchain.deployment.v1.OrchestrationSiteInfo;

import lombok.Builder;
import lombok.Data;

/**
 * Data object which stores session.
 */
@Data
@Builder
public class DeploymentExecutionContext {

    final UUID id;
    final UUID consortiumId;
    final UUID blockchainId;
    final BlockchainType blockchainType;
    final Map<OrchestrationSiteIdentifier, OrchestrationSiteInfo> sitesById;
    final Map<OrchestrationSiteIdentifier, Orchestrator> orchestrators;
    final OrchestrationSiteInfo.Type deploymentType;
    final Map<UUID, List<ConcordComponent>> componentsByNode;
    final NodeAssignment nodeAssignment;
    final ConcurrentHashMap.KeySetView<DeployedResource, Boolean> results
            = ConcurrentHashMap.newKeySet();

    final List<DeploymentExecutionEvent> events = new ArrayList<>();

    Map<UUID, LocalNodeDetails> localNodeDetailsMap;

    DeploymentExecutionEvent.Status status;

    /**
     * Local info class for a given node.
     */
    @Data
    @Builder
    public static class LocalNodeDetails {
        String privateIp;
    }
}
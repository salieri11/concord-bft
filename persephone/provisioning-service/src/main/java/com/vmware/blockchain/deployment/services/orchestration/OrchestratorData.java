/*
 * Copyright (c) 2018-2019 VMware, Inc. All rights reserved. VMware Confidential
 */

package com.vmware.blockchain.deployment.services.orchestration;

import java.net.URI;
import java.util.Map;
import java.util.UUID;

import com.vmware.blockchain.deployment.v1.ConcordModelSpecification;
import com.vmware.blockchain.deployment.v1.ConfigurationSessionIdentifier;
import com.vmware.blockchain.deployment.v1.Endpoint;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.EqualsAndHashCode;
import lombok.NoArgsConstructor;

/**
 * Holds all the data classes.
 */
public class OrchestratorData {

    /**
     * Generic interface type denoting a deployment event.
     */
    public abstract static class OrchestrationEvent {

    }

    /**
     * Temp v2 request.
     */
    @Data
    @AllArgsConstructor
    public static class CreateComputeResourceRequestV2 {

        UUID blockchainId;
        UUID nodeId;
        CloudInitData cloudInitData;
        Map<String, String> properties;

        /**
         * Temp v2 request.
         */
        @Data
        @AllArgsConstructor
        public static class CloudInitData {
            Endpoint containerRegistry;
            ConcordModelSpecification model;
            String privateIp;
            ConfigurationSessionIdentifier configGenId;
            Endpoint configServiceEndpoint;
            Endpoint configServiceRestEndpoint;
        }

        public String getVmId() {
            return blockchainId + "-" + nodeId;
        }
    }

    /**
     * Delete Resource request.
     */
    @Data
    @AllArgsConstructor
    public static class DeleteComputeResourceRequest {

        URI resource;
    }

    /**
     * Network resource provisioning request specification.
     *
     * @param[name] name of the network resource to create, for [Orchestrator] backends that associate network resource
     * with additional naming identifiers or surrogate identifiers to each provisioned network resource.
     * @param[publicResource] whether network resource must be externally reachable outside of the orchestration site.
     */
    @Data
    @AllArgsConstructor
    public static class CreateNetworkResourceRequest {

        String name;
        Boolean publicResource;
    }

    /**
     * Events corresponding to the execution of a network address requisition workflow.
     */
    @Data
    @AllArgsConstructor
    @NoArgsConstructor
    @EqualsAndHashCode(callSuper = false)
    public static class NetworkResourceEvent extends OrchestrationEvent {

        URI resource;
    }

    /**
     * Network resource created.
     */
    @Data
    @EqualsAndHashCode(callSuper = true, doNotUseGetters = true)
    public static final class NetworkResourceEventCreated extends NetworkResourceEvent {

        String name;
        String address;
        boolean publicResource;

        /**
         * Builder.
         */
        @Builder
        public NetworkResourceEventCreated(URI resource, String name,
                                           String address, boolean publicResource) {
            super(resource);
            this.name = name;
            this.address = address;
            this.publicResource = publicResource;
        }
    }

    /**
     * Network resource de-provisioning request specification.
     */
    @Data
    @AllArgsConstructor
    public static class DeleteNetworkResourceRequest {

        URI resource;
    }

    /**
     * Events corresponding to the execution of a deployment session.
     */
    @Data
    @AllArgsConstructor
    @NoArgsConstructor
    @EqualsAndHashCode(callSuper = false)
    public static class ComputeResourceEvent extends OrchestrationEvent {
        URI resource;
    }

    /**
     * Temp v2 request.
     */
    @Data
    @EqualsAndHashCode(callSuper = true, doNotUseGetters = true)
    public static final class ComputeResourceEventCreatedV2 extends ComputeResourceEvent {

        UUID nodeId;
        String nodePassword;

        /**
         * Temp v2 request.
         */
        @Builder
        public ComputeResourceEventCreatedV2(URI resource, UUID nodeId, String password) {
            super(resource);
            this.nodeId = nodeId;
            this.nodePassword = password;
        }
    }

    /**
     * Allocation request to assign a network resource to a given deployment.
     *
     * @param[name] name of the network allocation to create, for [Orchestrator] backends that associate network
     * allocation with additional naming identifiers or surrogate identifiers to each provisioned network allocation.
     * @param[compute] compute resource to allocate network resource to.
     * @param[publicNetwork] network resource to be assigned.
     * @param[privateNetwork] network resource to be assigned.
     */
    @Data
    @AllArgsConstructor
    public static class CreateNetworkAllocationRequest {

        String name;
        URI compute;
        URI publicNetwork;
        URI privateNetwork;
    }

    /**
     * Allocation request to assign a network resource to a given deployment.
     */
    @Data
    @AllArgsConstructor
    public static class CreateNetworkAllocationRequestV2 {

        String name;
        String publicIp;
        String privateIp;
    }

    /**
     * Network allocation de-provisioning request specification.
     *
     * @param[resource] network allocation resource to be de-provisioned.
     */
    @Data
    @AllArgsConstructor
    public static class DeleteNetworkAllocationRequest {

        URI resource;
    }

    /**
     * Events corresponding to the execution of a network allocation workflow.
     */
    @Data
    @AllArgsConstructor
    @NoArgsConstructor
    @EqualsAndHashCode(callSuper = false)
    public static class NetworkAllocationEvent extends OrchestrationEvent {
        URI resource;
    }

}
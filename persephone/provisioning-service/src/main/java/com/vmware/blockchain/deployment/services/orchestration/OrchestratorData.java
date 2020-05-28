/*
 * Copyright (c) 2018-2019 VMware, Inc. All rights reserved. VMware Confidential
 */

package com.vmware.blockchain.deployment.services.orchestration;

import java.net.URI;

import com.vmware.blockchain.deployment.v1.ConcordClusterIdentifier;
import com.vmware.blockchain.deployment.v1.ConcordModelSpecification;
import com.vmware.blockchain.deployment.v1.ConcordNodeIdentifier;
import com.vmware.blockchain.deployment.v1.ConfigurationSessionIdentifier;
import com.vmware.blockchain.deployment.v1.Endpoint;
import com.vmware.blockchain.ethereum.type.Genesis;

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
     * Compute resource deployment creation request specification.
     *
     * @param[cluster] identifier of the cluster the deployed resource belongs to.
     * @param[node] identifier of the member node.
     * @param[model] metadata specification of versioned Concord model template to deploy.
     * @param[genesis] common genesis block information to be deployed on the resource.
     * @param[privateNetworkAddress] network address to be statically assigned on the compute resource.
     */
    @Data
    @AllArgsConstructor
    public static class CreateComputeResourceRequest {

        ConcordClusterIdentifier cluster;
        ConcordNodeIdentifier node;
        ConcordModelSpecification model;
        Genesis genesis;
        String privateNetworkAddress = "";
        ConfigurationSessionIdentifier configurationSessionIdentifier;
        int concordId;
        Endpoint configServiceEndpoint;
        Endpoint configServiceRestEndpoint;
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
                                           String address, Boolean publicResource) {
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
     * Compute resource creation event.
     */
    @Data
    @EqualsAndHashCode(callSuper = true, doNotUseGetters = true)
    public static final class ComputeResourceEventCreated extends ComputeResourceEvent {

        ConcordNodeIdentifier node;
        String nodePassword;

        /**
         * Constructor.
         */
        @Builder
        public ComputeResourceEventCreated(URI resource, ConcordNodeIdentifier node, String password) {
            super(resource);
            this.node = node;
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

    /**
     * Network allocation creation.
     */
    @Data
    @EqualsAndHashCode(callSuper = false)
    public static final class NetworkAllocationEventCreated extends NetworkAllocationEvent {

        String name;
        URI compute;
        URI publicNetwork;
        URI privateNetwork;

        /**
         * Constructor.
         */
        @Builder
        public NetworkAllocationEventCreated(URI resource, String name,
                                             URI compute, URI publicNetwork, URI privateNetwork) {
            super(resource);
            this.name = name;
            this.compute = compute;
            this.publicNetwork = publicNetwork;
            this.privateNetwork = privateNetwork;
        }
    }
}
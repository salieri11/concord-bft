/*
 * Copyright (c) 2020 VMware, Inc. All rights reserved. VMware Confidential
 */

package com.vmware.blockchain.castor.model;

import java.util.ArrayList;
import java.util.List;

import javax.validation.Valid;
import javax.validation.constraints.NotEmpty;

import lombok.Builder;
import lombok.EqualsAndHashCode;
import lombok.Getter;
import lombok.Setter;
import lombok.experimental.SuperBuilder;

/**
 * Deployment Descriptor for cloning.
 */
@Getter
@Setter
@Builder
@EqualsAndHashCode
public class ReconfigurationDescriptorModel implements DeploymentDescriptorModel {

    // List of zone ids on which the replicas should be deployed
    // These MUST match the zone name in the Infrastructure descriptor.
    @NotEmpty(message = "deployment.replicas.not.specified")
    @Valid
    private List<PopulatedReplica> populatedReplicas;

    @Valid
    private NodeSpecification replicaNodeSpec;

    // Read Only replicas are optional
    @Valid
    private List<ReadonlyReplica> readonlyReplicas;

    @Valid
    private NodeSpecification readonlyReplicaNodeSpec;

    @NotEmpty(message = "deployment.clients.not.specified")
    @Valid
    @Getter
    private List<PopulatedClient> populatedClients;

    @Valid
    private NodeSpecification clientNodeSpec;

    @Valid
    private Blockchain blockchain;

    @Valid
    private OperatorSpecifications operatorSpecifications;

    /**
     * Required client.
     */
    @Getter
    @Setter
    @SuperBuilder
    @EqualsAndHashCode
    public static class PopulatedClient extends Client {
        private String nodeId;
        private String damlDbPassword;
        private String clientGroupId;
    }

    /**
     * Required replicas.
     */
    @Getter
    @Setter
    @SuperBuilder
    @EqualsAndHashCode
    public static class PopulatedReplica extends Replica {
        private String nodeId;
    }

    /**
     * Required read only replicas.
     */
    @Getter
    @Setter
    @SuperBuilder
    @EqualsAndHashCode
    public static class PopulatedReadOnlyReplica extends ReadonlyReplica {
        private String nodeId;
    }

    /**
     * Get replica info.
     */
    public List<Replica> getReplicas() {
        if (populatedReplicas == null) {
            return new ArrayList<>();
        }
        return new ArrayList<>(populatedReplicas);
    }

    /**
     * Get clients info.
     */
    public List<Client> getClients() {
        if (populatedClients == null) {
            return new ArrayList<>();
        }
        return new ArrayList<>(populatedClients);
    }
}

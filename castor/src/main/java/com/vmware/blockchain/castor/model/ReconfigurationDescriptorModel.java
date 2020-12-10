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
    private List<Replica> replicas;

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
    @EqualsAndHashCode
    public static class PopulatedClient extends Client {
        private String damlDbPassword;
        private String clientGroupId;

        /**
         * Override builder for inheritance.
         */
        @Builder
        public PopulatedClient(String damlDbPassword, String clientGroupId,
                               String zoneName, String authUrlJwt, String providedIp, String groupName,
                               TlsLedgerData tlsLedgerData) {
            super(zoneName, authUrlJwt, providedIp, groupName, tlsLedgerData);
            this.damlDbPassword = damlDbPassword;
            this.clientGroupId = clientGroupId;
        }

        /**
         * Builder class. Do NOT Delete this. Compilation will fail even if Intellij says so.
         */
        public static class PopulatedClientBuilder extends ClientBuilder {
            PopulatedClientBuilder() {
                super();
            }
        }
    }

    public List<Client> getClients() {
        return new ArrayList<>(populatedClients);
    }
}

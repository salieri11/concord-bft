/*
 * Copyright (c) 2020 VMware, Inc. All rights reserved. VMware Confidential
 */

package com.vmware.blockchain.castor.model;

import java.util.List;
import java.util.UUID;

import javax.validation.constraints.NotBlank;

import com.vmware.blockchain.castor.service.BlockchainTypesValid;

import lombok.Builder;
import lombok.EqualsAndHashCode;
import lombok.Getter;
import lombok.Setter;

/**
 * The interface that defines contracts for the provision, deprovision, and reconfigure models.
 */
public interface DeploymentDescriptorModel {

    /**
     * Required blockchain type.
     * NOTE: These entries must be up-to-date with the protobuf definition.
     */
    public enum BlockchainType {
        ETHEREUM,
        DAML
    }

    /**
     * Required client.
     */
    @Getter
    @Setter
    @Builder
    @EqualsAndHashCode
    public static class Client {
        // This MUST match the zone name in the Infrastructure descriptor.
        @NotBlank(message = "deployment.client.zone.invalid")
        private String zoneName;
        private String authUrlJwt;
        private String providedIp;
        private String groupName;
    }

    /**
     * Required committer.
     */
    @Getter
    @Setter
    @Builder
    @EqualsAndHashCode
    public static class Committer {
        // This MUST match the zone name in the Infrastructure descriptor.
        @NotBlank(message = "deployment.committer.zone.invalid")
        private String zoneName;
        private String providedIp;
    }

    /**
     * Required blockchain.
     */
    @Getter
    @Setter
    @Builder
    @EqualsAndHashCode
    public static class Blockchain {
        String consortiumName;
        @BlockchainTypesValid(allowedTypes = {BlockchainType.DAML, BlockchainType.ETHEREUM})
        BlockchainType blockchainType;
        UUID blockchainId;
    }

    /**
     * Get committers from the deployment model.
     * @return a list of committers
     */
    List<Committer> getCommitters();

    /**
     * Get clients from the deployment model.
     * @return a list of clients
     */
    List<Client> getClients();

    /**
     * Get the blockchain spec from the deployment model.
     * @return a blockchain
     */
    Blockchain getBlockchain();

}

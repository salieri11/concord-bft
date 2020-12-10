/*
 * Copyright (c) 2020 VMware, Inc. All rights reserved. VMware Confidential
 */

package com.vmware.blockchain.castor.model;

import java.util.UUID;

import com.vmware.blockchain.castor.service.BlockchainTypesValid;

import lombok.Builder;
import lombok.EqualsAndHashCode;
import lombok.Getter;
import lombok.Setter;

/**
 * The interface that defines contracts for the provision, deprovision, and reconfigure models.
 */
public interface DeploymentDescriptorModel extends ReplicaDescriptorModel, ClientDescriptorModel {

    /**
     * Required blockchain type.
     * NOTE: These entries must be up-to-date with the protobuf definition.
     */
    enum BlockchainType {
        ETHEREUM,
        DAML
    }

    /**
     * Required blockchain.
     */
    @Getter
    @Setter
    @Builder
    @EqualsAndHashCode
    class Blockchain {
        String consortiumName;
        @BlockchainTypesValid(allowedTypes = {BlockchainType.DAML, BlockchainType.ETHEREUM})
        BlockchainType blockchainType;
        UUID blockchainId;
    }

    /**
     * Get the blockchain spec from the deployment model.
     * @return a blockchain
     */
    Blockchain getBlockchain();

    /**
     * Get the attributes for Blockchain operator.
     * @return a spec
     */
    OperatorSpecifications getOperatorSpecifications();

}

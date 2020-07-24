/*
 * Copyright (c) 2020 VMware, Inc. All rights reserved. VMware Confidential
 */

package com.vmware.blockchain.castor.model;

import java.util.List;

import lombok.Builder;
import lombok.EqualsAndHashCode;
import lombok.Getter;
import lombok.Setter;

/**
 * Deployment Descriptor. Contains properties that are specific to each blockchain deployment.
 */
@Getter
@Setter
@Builder
@EqualsAndHashCode
public class DeploymentDescriptorModel {

    /**
     * Required client.
     */
    @Getter
    @Setter
    @Builder
    @EqualsAndHashCode
    public static class Client {
        // This MUST match the zone name in the Infrastructure descriptor.
        // DINKARTODO: add validation for this condition.
        private String zoneName;
        private String authUrlJwt;
    }

    /**
     * Required blockchain type.
     * NOTE: These entries must be up-to-date with the protobuf definition.
     */
    public enum BlockchainType {
        ETHEREUM,
        DAML,
        HLF
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
        BlockchainType bockchainType;
    }

    // List of zone ids on which the committers should be deployed
    // These MUST match the zone name in the Infrastructure descriptor.
    // DINKARTODO: add validation for this condition.
    private List<String> committers;

    private List<Client> clients;
    private Blockchain blockchain;
}

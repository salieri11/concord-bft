/*
 * Copyright (c) 2020 VMware, Inc. All rights reserved. VMware Confidential
 */

package com.vmware.blockchain.castor.model;

import java.util.List;
import java.util.UUID;

import javax.validation.constraints.Max;
import javax.validation.constraints.Min;
import javax.validation.constraints.NotBlank;

import com.vmware.blockchain.castor.service.BlockchainTypesValid;
import com.vmware.blockchain.castor.service.LedgerTlsClientAuthValid;

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
        private LedgerTls ledgerTls;
    }

    /**
     * Optional TLS settings for client.
     */
    @Getter
    @Setter
    @Builder
    @EqualsAndHashCode
    class LedgerTls {
        private String pem;     // The pem file to be used as the private key.
        private String crt;     // The crt file to be used as the cert chain.
        private String cacrt;   // The crt file to be used as the the trusted root CA.

        @Builder.Default
        @LedgerTlsClientAuthValid(allowedTypes = {ClientAuth.NONE, ClientAuth.OPTIONAL, ClientAuth.REQUIRE})
        private ClientAuth clientAuth = ClientAuth.REQUIRE; // Based on DAML SDK docs.

        public enum ClientAuth {
            NONE,
            OPTIONAL,
            REQUIRE
        }
    }

    /**
     * Required replica.
     */
    @Getter
    @Setter
    @Builder
    @EqualsAndHashCode
    public static class Replica {
        // This MUST match the zone name in the Infrastructure descriptor.
        @NotBlank(message = "deployment.replica.zone.invalid")
        private String zoneName;
        private String providedIp;
    }

    /**
     * Defines node size.
     */
    @Getter
    @Setter
    @Builder
    @EqualsAndHashCode
    public static class NodeSpecification {

        @Min(value = 2, message = "invalid.mincpu")
        @Max(value = 32, message = "invalid.maxcpu")
        private int cpuCount;

        @Min(value = 16, message = "invalid.minmemory")
        @Max(value = 128, message = "invalid.maxmemory")
        private int memoryGb;

        @Min(value = 64, message = "invalid.mindisk")
        @Max(value = 62 * 4096, message = "invalid.maxdisk")
        private int diskSizeGb;
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
     * Get replica node specification from deployment model.
     * @return node spec
     */
    NodeSpecification getReplicaNodeSpec();


    /**
     * Get replicas from the deployment model.
     * @return a list of replicas
     */
    List<Replica> getReplicas();

    /**
     * Get clients from the deployment model.
     * @return a list of clients
     */
    List<Client> getClients();

    /**
     * Get client node specification from deployment model.
     * @return node spec
     */
    NodeSpecification getClientNodeSpec();


    /**
     * Get the blockchain spec from the deployment model.
     * @return a blockchain
     */
    Blockchain getBlockchain();

}

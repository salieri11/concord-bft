/*
 * Copyright (c) 2020 VMware, Inc. All rights reserved. VMware Confidential
 */

package com.vmware.blockchain.castor.model;

import java.util.List;

import javax.validation.constraints.NotBlank;

import com.vmware.blockchain.castor.service.LedgerTlsClientAuthValid;

import lombok.Builder;
import lombok.EqualsAndHashCode;
import lombok.Getter;
import lombok.Setter;
import lombok.experimental.SuperBuilder;

/**
 * The interface that defines contracts for the provision, deprovision, and reconfigure models.
 */
public interface ClientDescriptorModel {

    /**
     * Required client.
     */
    @Getter
    @Setter
    @SuperBuilder
    @EqualsAndHashCode
    public static class Client {
        // This MUST match the zone name in the Infrastructure descriptor.
        @NotBlank(message = "deployment.client.zone.invalid")
        private String zoneName;
        private String authUrlJwt;
        private String providedIp;
        private String groupName;
        private TlsLedgerData tlsLedgerData;
    }

    /**
     * Optional TLS settings for client.
     */
    @Getter
    @Setter
    @Builder
    @EqualsAndHashCode
    class TlsLedgerData {
        private String pem;     // The pem file to be used as the private key.
        private String crt;     // The crt file to be used as the cert chain.
        private String cacrt;   // The crt file to be used as the the trusted root CA.

        /**
         * Client auth level for mTLS.
         */
        public enum ClientAuth {
            NONE,
            OPTIONAL,
            REQUIRE
        }

        @Builder.Default
        @LedgerTlsClientAuthValid(allowedTypes = {ClientAuth.NONE, ClientAuth.OPTIONAL, ClientAuth.REQUIRE})
        private ClientAuth clientAuth = ClientAuth.REQUIRE; // Based on DAML SDK docs.
    }


    /**
     * Get clients from the deployment model.
     * @return a list of clients
     */
    List<Client> getClients();

    /**
     * Get client node specification from deployment model.
     * @return node spec
     */
    ReplicaDescriptorModel.NodeSpecification getClientNodeSpec();

}

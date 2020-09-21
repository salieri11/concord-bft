/*
 * Copyright (c) 2019 VMware, Inc. All rights reserved. VMware Confidential
 */

package com.vmware.blockchain.services.blockchains;

import static com.vmware.blockchain.services.blockchains.Blockchain.BlockchainType;

import java.util.Date;
import java.util.HashMap;
import java.util.List;
import java.util.UUID;

import javax.validation.Valid;
import javax.validation.constraints.NotNull;

import org.springframework.web.bind.annotation.RestController;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.vmware.blockchain.services.blockchains.nodesizing.NodeSizeTemplate;

import lombok.AllArgsConstructor;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;

/**
 * Controller to create and list blockchains.
 */
@RestController
public class BlockchainApiObjects {

    @Getter
    @Setter
    @Valid
    @JsonIgnoreProperties(ignoreUnknown = true)
    static class BlockchainPost {
        @NotNull(message = "Consortium ID cannot be empty")
        private UUID consortiumId;

        @NotNull(message = "Blockchain type cannot be empty")
        private BlockchainType blockchainType;

        @Deprecated
        private List<UUID> replicaZoneIds;

        private List<ReplicaNodeRequest> replicaNodes;

        private List<ClientNodeRequest> clientNodes;
    }

    @Getter
    @Setter
    @Valid
    @JsonIgnoreProperties(ignoreUnknown = true)
    static class ReplicaNodeRequest {
        private UUID zoneId;
        private HashMap<NodeSizeTemplate.Parameter, String> sizingInfo;
    }

    // Can extract a base node type.
    @Getter
    @Setter
    @Valid
    @JsonIgnoreProperties(ignoreUnknown = true)
    static class ClientNodeRequest {
        private UUID zoneId;
        private String authUrlJwt;
        private String groupName;
        private HashMap<NodeSizeTemplate.Parameter, String> sizingInfo;

        // TLS fields.
        private String pem;     // The pem file to be used as the private key.
        private String crt;     // The crt file to be used as the cert chain.
        private String cacrt;   // The crt file to be used as the the trusted root CA.
    }

    @Getter
    @Setter
    @JsonIgnoreProperties(ignoreUnknown = true)
    static class BlockchainPatch {
        private String ipList;
        private String rpcUrls;
        private String rpcCerts;
    }

    @Getter
    @Setter
    @NoArgsConstructor
    @AllArgsConstructor
    static class BlockchainGetResponse {
        private UUID id;
        private UUID consortiumId;
        private BlockchainType blockchainType;
        private Blockchain.BlockchainState blockchainState;
        private String version;
        private String createdBy;
        private Date created;

        public BlockchainGetResponse(Blockchain b) {
            this.id = b.getId();
            this.consortiumId = b.getConsortium();
            this.blockchainType = b.getType() == null ? BlockchainType.ETHEREUM : b.getType();
            this.blockchainState = b.getState() == null ? Blockchain.BlockchainState.ACTIVE : b.getState();
            this.version = b.getBlockchainVersionString();
            this.createdBy = b.getCreateUserName();
            this.created = b.getCreated();
        }
    }

    /**
     * Response from blockchain post, with a task id.
     */
    @Getter
    @Setter
    @AllArgsConstructor
    @NoArgsConstructor
    public static class BlockchainTaskResponse {
        private UUID taskId;
    }

}

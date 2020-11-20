/*
 * Copyright (c) 2019 VMware, Inc. All rights reserved. VMware Confidential
 */

package com.vmware.blockchain.services.blockchains.clients;

import java.util.List;
import java.util.Optional;
import java.util.UUID;
import java.util.stream.Collectors;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.security.access.prepost.PreAuthorize;
import org.springframework.web.bind.annotation.PatchMapping;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestMethod;
import org.springframework.web.bind.annotation.RestController;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.google.common.base.Strings;
import com.vmware.blockchain.common.ErrorCode;
import com.vmware.blockchain.common.NotFoundException;
import com.vmware.blockchain.services.blockchains.Blockchain;
import com.vmware.blockchain.services.blockchains.BlockchainService;
import com.vmware.blockchain.services.blockchains.replicas.Replica;
import com.vmware.blockchain.services.blockchains.replicas.ReplicaService;
import com.vmware.blockchain.services.models.NodeGetCredentialsResponse;

import lombok.AllArgsConstructor;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;

/**
 * Controller to create and list clients.
 */
@RestController
@RequestMapping(path = "/api/blockchains/{bid}/clients")
public class ClientController {
    private  static final Logger logger = LogManager.getLogger(ClientController.class);

    /**
     * Class for ClientGetResponse.
     */
    @Getter
    @Setter
    @NoArgsConstructor
    @AllArgsConstructor
    public static class ClientGetResponse {
        private UUID id;
        private String publicIp;
        private String privateIp;
        private String name;
        private String url;
        private String cert;
        private UUID zoneId;
        private String authUrlJwt;
        private UUID groupId;
        private String groupName;

        /**
         * Constructor for ClientGetResponse Class.
         * @param r Replica.
         */
        @Deprecated
        public ClientGetResponse(Replica r) {
            this.id = r.getId();
            this.publicIp = r.getPublicIp();
            this.privateIp = r.getPrivateIp();
            this.name = r.getHostName();
            this.url = r.getUrl();
            this.cert = r.getCert();
            this.zoneId = r.getZoneId();
        }

        /**
         * Constructor for ClientGetResponse Class.
         * @param client Client.
         */
        public ClientGetResponse(Client client) {
            this.id = client.getId();
            this.publicIp = client.getPublicIp();
            this.privateIp = client.getPrivateIp();
            this.url = client.getUrl();
            this.zoneId = client.getZoneId();
            this.authUrlJwt = client.getAuthJwtUrl();
            this.groupId = client.getGroupId();
            // Group Name can be null, if user did not pick one during BC creation.
            if (!Strings.isNullOrEmpty(client.getGroupName())) {
                this.groupName = client.getGroupName();
            }
        }
    }

    /**
     * Class for ClientGetTlsCredentials.
     */
    @Getter
    @Setter
    @NoArgsConstructor
    @AllArgsConstructor
    public static class ClientGetTlsCredentialsResponse {
        private String pem;
        private String crt;
        private String cacrt;
    }

    @Getter
    @Setter
    @JsonIgnoreProperties(ignoreUnknown = true)
    static class ClientPatch {
        private String damlDbPassword;
    }

    @Getter
    @Setter
    @NoArgsConstructor
    @AllArgsConstructor
    @JsonIgnoreProperties(ignoreUnknown = true)
    static class ClientPatchResponse {
        private UUID id;
        String publicIp;
        String privateIp;
        String password;
        String url;
        String authJwtUrl;
        String damlDbPassword;
        UUID blockchainId;
        UUID zoneId;
        UUID groupId;
        String groupName;
        String pem;
        String crt;
        String cacrt;

        ClientPatchResponse(Client client) {
            this.id = client.getId();
            this.publicIp = client.getPublicIp();
            this.privateIp = client.getPrivateIp();
            this.password = client.getPassword();
            this.url = client.getUrl();
            this.authJwtUrl = client.getAuthJwtUrl();
            this.damlDbPassword = client.getDamlDbPassword();
            this.blockchainId = client.getBlockchainId();
            this.zoneId = client.getZoneId();
            this.groupId = client.getGroupId();
            this.groupName = client.getGroupName();
            this.pem = client.getPem();
            this.crt = client.getCrt();
            this.cacrt = client.getCacrt();
        }
    }

    @Deprecated
    private ReplicaService replicaService;

    private ClientService clientService;
    private BlockchainService blockchainService;

    @Autowired
    public ClientController(ReplicaService replicaService,
                            ClientService clientService,
                            BlockchainService blockchainService) {
        this.replicaService = replicaService;
        this.clientService = clientService;
        this.blockchainService = blockchainService;
    }

    /**
     * Get the list of all participant nodes.
     */
    @RequestMapping(path = "", method = RequestMethod.GET)
    @PreAuthorize("@authHelper.isUser()")
    ResponseEntity<List<ClientGetResponse>> listParticipants(@PathVariable("bid") UUID bid) throws NotFoundException {
        safeGetBlockchain(bid);

        var clients = clientService.getClientsByParentId(bid);
        List<ClientGetResponse> clientGetResponseList;
        if (clients == null || clients.isEmpty()) {
            logger.warn("Using older flow for blockchain {}", bid);
            // Backward compatible.
            clientGetResponseList = replicaService.getReplicas(bid)
                    .stream()
                    .filter(replica -> replica.getReplicaType() == Replica.ReplicaType.DAML_PARTICIPANT)
                    .map(ClientGetResponse::new)
                    .collect(Collectors.toList());
        } else {
            clientGetResponseList = clients
                    .stream()
                    .map(ClientGetResponse::new)
                    .collect(Collectors.toList());

        }
        return new ResponseEntity<>(clientGetResponseList, HttpStatus.OK);
    }

    /**
     * Get credentials for a given replica.
     * @param bid       Blockchain ID
     * @param clientId  Client ID for which credentials are requested
     * @return          200 with ClientGetCredentialsResponse
     */
    @RequestMapping(method = RequestMethod.GET, path = {"/{clientId}/credentials"})
    @PreAuthorize("@authHelper.isConsortiumParticipant()")
    public ResponseEntity<NodeGetCredentialsResponse> getClientCredentials(@PathVariable UUID bid,
                                                                            @PathVariable UUID clientId) {
        safeGetBlockchain(bid);

        Client client = safeClientGet(bid, clientId);

        NodeGetCredentialsResponse nodeGetCredentialsResponse = new NodeGetCredentialsResponse("root",
                client.getPassword());

        return new ResponseEntity<>(nodeGetCredentialsResponse, HttpStatus.OK);
    }

    /**
     * Get credentials for a given replica.
     * @param bid       Blockchain ID
     * @param clientId  Client ID for which mTLS credentials are requested
     * @return          200 with ClientGetCredentialsResponse
     */
    @RequestMapping(method = RequestMethod.GET, path = {"/{clientId}/daml-certificates"})
    @PreAuthorize("@authHelper.isConsortiumParticipant()")
    public ResponseEntity<ClientGetTlsCredentialsResponse> getClientTlsCredentials(@PathVariable UUID bid,
                                                                           @PathVariable UUID clientId) {
        safeGetBlockchain(bid);

        Client client = safeClientGet(bid, clientId);

        ClientGetTlsCredentialsResponse clientGetTlsCredentials = new ClientGetTlsCredentialsResponse(
                client.getPem(),
                client.getCrt(),
                client.getCacrt()
        );

        return new ResponseEntity<>(clientGetTlsCredentials, HttpStatus.OK);
    }

    /**
     * Updates client, currently only DAML DB password can be updated.
     * @param bid       Blockchain ID
     * @param clientId  Client ID for which credentials are requested
     * @return          202
     */
    @PatchMapping(path = {"/{clientId}"})
    @PreAuthorize("@authHelper.isConsortiumParticipant()")
    public ResponseEntity<ClientPatchResponse> updateClient(@PathVariable UUID bid, @PathVariable UUID clientId,
                                                                   @RequestBody ClientPatch newClientDetails) {
        safeGetBlockchain(bid);

        Client client = safeClientGet(bid, clientId);

        ClientPatchResponse cpr = new ClientPatchResponse(clientService.updateClient(client, newClientDetails));

        return new ResponseEntity<>(cpr, HttpStatus.ACCEPTED);
    }

    // This could be done better, but maybe later. We need to handle errors first.
    private Blockchain safeGetBlockchain(UUID bid) {
        Blockchain blockchain;
        try {
            blockchain = blockchainService.get(bid);
            if (blockchain == null) {
                throw new NotFoundException(ErrorCode.BLOCKCHAIN_NOT_FOUND, bid.toString());
            }
        } catch (NotFoundException e) {
            throw new NotFoundException(ErrorCode.BLOCKCHAIN_NOT_FOUND, bid.toString());
        }

        return blockchain;
    }

    // This could be done better, but maybe later. We need to handle errors first.
    private Client safeClientGet(UUID blockchainId, UUID clientId) {
        Optional<Client> clientOpt = clientService.getClientsByParentId(blockchainId).stream()
                .filter(c -> c.getId().equals(clientId)).findFirst();
        if (clientOpt.isEmpty()) {
            throw new NotFoundException(ErrorCode.CLIENT_NOT_FOUND, clientId.toString(),
                    blockchainId.toString());
        }

        return clientOpt.get();
    }
}

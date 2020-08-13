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
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestMethod;
import org.springframework.web.bind.annotation.RestController;

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
        Blockchain b = blockchainService.get(bid);
        if (b == null) {
            throw new NotFoundException(String.format("Blockchain %s does not exist.", bid.toString()));
        }

        var clients = clientService.getClientsByParentId(bid);
        List<ClientGetResponse> replicaGetResponseList;
        if (clients == null || clients.isEmpty()) {
            logger.warn("Using older flow for blockchain {}", bid);
            // Backward compatible.
            replicaGetResponseList = replicaService.getReplicas(bid)
                    .stream()
                    .filter(replica -> replica.getReplicaType() == Replica.ReplicaType.DAML_PARTICIPANT)
                    .map(ClientGetResponse::new)
                    .collect(Collectors.toList());
        } else {
            replicaGetResponseList = clients
                    .stream()
                    .map(ClientGetResponse::new)
                    .collect(Collectors.toList());

        }
        return new ResponseEntity<>(replicaGetResponseList, HttpStatus.OK);
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
        Blockchain b = blockchainService.get(bid);
        if (b == null) {
            throw new NotFoundException(String.format("Blockchain %s does not exist.", bid.toString()));
        }

        Optional<Client> clientOpt = clientService.getClientsByParentId(bid).stream()
                .filter(c -> c.getId().equals(clientId)).findFirst();
        if (clientOpt.isEmpty()) {
            throw new NotFoundException(ErrorCode.NOT_FOUND);
        }
        Client client = clientOpt.get();

        return new ResponseEntity<>(NodeGetCredentialsResponse.builder()
                .username("root").password(client.password).build(), HttpStatus.OK);
    }

}

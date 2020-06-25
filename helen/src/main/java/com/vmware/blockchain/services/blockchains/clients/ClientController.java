/*
 * Copyright (c) 2019 VMware, Inc. All rights reserved. VMware Confidential
 */

package com.vmware.blockchain.services.blockchains.clients;

import java.util.List;
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

import com.vmware.blockchain.common.NotFoundException;
import com.vmware.blockchain.services.blockchains.Blockchain;
import com.vmware.blockchain.services.blockchains.BlockchainService;
import com.vmware.blockchain.services.blockchains.replicas.Replica;
import com.vmware.blockchain.services.blockchains.replicas.ReplicaService;

import lombok.AllArgsConstructor;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;

/**
 * Controller to create and list clients.
 */
@RestController
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
         * @param client.
         */
        public ClientGetResponse(Client client) {
            this.id = client.getId();
            this.publicIp = client.getPublicIp();
            this.privateIp = client.getPrivateIp();
            this.url = client.getUrl();
            this.zoneId = client.getZoneId();
            this.authUrlJwt = client.getAuthJwtUrl();
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
    @RequestMapping(path = "/api/blockchains/{bid}/clients", method = RequestMethod.GET)
    @PreAuthorize("@authHelper.isUser()")
    ResponseEntity<List<ClientGetResponse>> listParticipants(@PathVariable("bid") UUID bid) throws NotFoundException {
        Blockchain b = blockchainService.get(bid);
        if (b == null) {
            throw new NotFoundException(String.format("Blockchain %s does not exist.", bid.toString()));
        }

        var clients = clientService.getClientsByBlockchainId(bid);
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

}

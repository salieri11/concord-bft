/*
 * Copyright (c) 2018-2019 VMware, Inc. All rights reserved. VMware Confidential
 */

package com.vmware.blockchain.services.concord;

import java.nio.charset.StandardCharsets;
import java.nio.file.FileSystems;
import java.nio.file.Files;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.UUID;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.json.simple.JSONArray;
import org.json.simple.JSONAware;
import org.json.simple.JSONObject;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.ResponseEntity;
import org.springframework.stereotype.Controller;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestMethod;
import org.springframework.web.bind.annotation.RequestParam;

import com.vmware.blockchain.connections.ConnectionPoolManager;
import com.vmware.blockchain.services.ConcordServlet;
import com.vmware.blockchain.services.profiles.Blockchain;
import com.vmware.blockchain.services.profiles.BlockchainService;
import com.vmware.blockchain.services.profiles.DefaultProfiles;
import com.vmware.concord.Concord;

/**
 * Controller for member list.
 */
@Controller
public final class MemberList extends ConcordServlet {
    private static final long serialVersionUID = 1L;
    private static final Logger logger = LogManager.getLogger(MemberList.class);
    private BlockchainService blockchainService;

    // This is a hack to allow us to pass request details though the sendToconcordAndBuildHelenResponse flow, without
    // including them in the communication with concord.
    private ThreadLocal<Boolean> includeRpcCerts = new ThreadLocal<Boolean>();

    @Autowired
    public MemberList(ConnectionPoolManager connectionPoolManager, DefaultProfiles defaultProfiles,
            BlockchainService blockchainService) {
        super(connectionPoolManager, defaultProfiles);
        this.blockchainService = blockchainService;

    }

    /**
     * Services a get request. Constructs a protobuf request of type peer request (enveloped in an concord request) as
     * defined in concord.proto. Sends this request to Concord. Parses the response and converts it into json for
     * responding to the client.
     *
     */
    @RequestMapping(method = RequestMethod.GET,
            path = {"/api/concord/members", "/api/blockchains/{id}/concord/members"})
    public ResponseEntity<JSONAware> doGet(
        @PathVariable(name = "id", required = false) Optional<UUID> id,
        @RequestParam(name = "certs", defaultValue = "false") String includeRpcCerts) {
        // stash the certs param for use during response construction
        this.includeRpcCerts.set(Boolean.valueOf(includeRpcCerts));

        // Construct a peer request object. Set its return_peers field.
        final Concord.PeerRequest peerRequestObj = Concord.PeerRequest.newBuilder().setReturnPeers(true).build();

        // Envelope the peer request object into an concord object.
        final Concord.ConcordRequest concordrequestObj =
                Concord.ConcordRequest.newBuilder().setPeerRequest(peerRequestObj).build();

        return getHelper(id).sendToConcordAndBuildHelenResponse(concordrequestObj);
    }

    /**
     * Parses the Protocol Buffer response from Concord and converts it into JSON. Method overridden because the
     * response for this API is of type JSONArray as opposed to JSONObject.
     *
     * @param concordResponse Protocol Buffer object containing Concord's reponse
     * @return Response in JSON format
     */
    public JSONAware parseToJson(UUID blockchain, Concord.ConcordResponse concordResponse) {
        // Extract the peer response from the concord reponse envelope.
        Concord.PeerResponse peerResponse = concordResponse.getPeerResponse();

        // Read list of peer objects from the peer response object.
        List<Concord.Peer> peerList = peerResponse.getPeerList();
        JSONArray peerArr = new JSONArray();
        Blockchain bc = blockchainService.get(blockchain);

        Map<String, String> rpcUrls = bc.getUrlsAsMap();
        Map<String, String> rpcCerts = bc.getCertsAsMap();

        // Iterate through each peer and construct
        // a corresponding JSON object
        for (Concord.Peer peer : peerList) {
            JSONObject peerJson = new JSONObject();
            String hostname = peer.getHostname();
            peerJson.put("hostname", hostname);
            peerJson.put("address", peer.getAddress());
            peerJson.put("status", peer.getStatus());
            peerJson.put("millis_since_last_message", peer.getMillisSinceLastMessage());
            peerJson.put("millis_since_last_message_threshold", peer.getMillisSinceLastMessageThreshold());
            peerJson.put("rpc_url", rpcUrls.getOrDefault(hostname, ""));
            if (includeRpcCerts.get()) {
                peerJson.put("rpc_cert", readCertFile(rpcCerts.get(hostname)));
            }

            // Store into a JSON array of all peers.
            peerArr.add(peerJson);
        }

        return peerArr;
    }

    /**
     * Read the certificate file, for inclusion in the JSON response. If the argument is null, or any error occurs,
     * return an empty string.
     */
    private String readCertFile(String filename) {
        if (filename == null) {
            return "";
        }

        try {
            // TODO: caching
            byte[] certBytes = Files.readAllBytes(FileSystems.getDefault().getPath(filename));
            return new String(certBytes, StandardCharsets.UTF_8);
        } catch (Exception e) {
            logger.warn("Problem reading cert file '" + filename + "'", e);
            return "";
        }
    }
}

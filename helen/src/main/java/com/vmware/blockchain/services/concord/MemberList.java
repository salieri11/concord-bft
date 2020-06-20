/*
 * Copyright (c) 2018-2019 VMware, Inc. All rights reserved. VMware Confidential
 */

package com.vmware.blockchain.services.concord;

import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.UUID;
import java.util.stream.Collectors;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.json.simple.JSONArray;
import org.json.simple.JSONAware;
import org.json.simple.JSONObject;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.ResponseEntity;
import org.springframework.security.access.prepost.PreAuthorize;
import org.springframework.stereotype.Controller;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestMethod;
import org.springframework.web.bind.annotation.RequestParam;

import com.vmware.blockchain.connections.ConnectionPoolManager;
import com.vmware.blockchain.services.ConcordServlet;
import com.vmware.blockchain.services.blockchains.replicas.ReplicaService;
import com.vmware.blockchain.services.profiles.DefaultProfiles;
import com.vmware.concord.Concord;

/**
 * Controller for member list.
 */
@Controller
public class MemberList extends ConcordServlet {
    private static final long serialVersionUID = 1L;
    private static final Logger logger = LogManager.getLogger(MemberList.class);
    private ReplicaService replicaService;

    // This is a hack to allow us to pass request details though the sendToconcordAndBuildHelenResponse flow, without
    // including them in the communication with concord.
    private ThreadLocal<Boolean> includeRpcCerts = new ThreadLocal<Boolean>();

    @Autowired
    public MemberList(ConnectionPoolManager connectionPoolManager, DefaultProfiles defaultProfiles,
                      ReplicaService replicaService) {
        super(connectionPoolManager, defaultProfiles);
        this.replicaService = replicaService;

    }

    /**
     * Services a get request. Constructs a protobuf request of type peer request (enveloped in an concord request) as
     * defined in concord.proto. Sends this request to Concord. Parses the response and converts it into json for
     * responding to the client.
     *
     */
    @RequestMapping(method = RequestMethod.GET,
            path = {"/api/concord/members", "/api/blockchains/{id}/concord/members"})
    @PreAuthorize("@authHelper.canAccessChain(#id)")
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
        // Extract the peer response from the concord response envelope.
        Concord.PeerResponse peerResponse = concordResponse.getPeerResponse();

        // Read list of peer objects from the peer response object.
        List<Concord.Peer> peerList = peerResponse.getPeerList();
        JSONArray peerArr = new JSONArray();
        var replicas = replicaService.getReplicas(blockchain);


        Map<String, String> rpcUrls =
                replicas.stream().collect(Collectors.toMap(n -> n.getHostName(), n -> n.getUrl()));
        Map<String, String> rpcCerts =
                replicas.stream().collect(Collectors.toMap(n -> n.getHostName(), n -> n.getCert()));

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
            peerJson.put("rpc_url", rpcUrls.getOrDefault(peer.getHostname(), ""));
            if (includeRpcCerts.get()) {
                peerJson.put("rpc_cert", rpcCerts.get(peer.getHostname()));
            }

            // Store into a JSON array of all peers.
            peerArr.add(peerJson);
        }

        return peerArr;
    }

}

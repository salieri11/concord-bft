/*
 * Copyright (c) 2018 VMware, Inc. All rights reserved. VMware Confidential
 */

package com.vmware.blockchain.services.concord;

import java.util.List;
import java.util.Map;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.json.simple.JSONArray;
import org.json.simple.JSONAware;
import org.json.simple.JSONObject;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.ResponseEntity;
import org.springframework.stereotype.Controller;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestMethod;

import com.vmware.blockchain.common.ConcordProperties;
import com.vmware.blockchain.connections.ConcordConnectionPool;
import com.vmware.blockchain.services.BaseServlet;
import com.vmware.concord.Concord;

/**
 * Controller for member list.
 */
@Controller
public final class MemberList extends BaseServlet {
    private static final long serialVersionUID = 1L;
    private static final Logger logger = LogManager.getLogger(MemberList.class);

    @Autowired
    public MemberList(ConcordProperties config, ConcordConnectionPool concordConnectionPool) {
        super(config, concordConnectionPool);

    }

    /**
     * Services a get request. Constructs a protobuf request of type peer request (enveloped in an concord request) as
     * defined in concord.proto. Sends this request to Concord. Parses the response and converts it into json for
     * responding to the client.
     *
     */
    @RequestMapping(method = RequestMethod.GET, path = "/api/concord/members")
    public ResponseEntity<JSONAware> doGet() {
        // Construct a peer request object. Set its return_peers field.
        final Concord.PeerRequest peerRequestObj = Concord.PeerRequest.newBuilder().setReturnPeers(true).build();

        // Envelope the peer request object into an concord object.
        final Concord.ConcordRequest concordrequestObj =
                Concord.ConcordRequest.newBuilder().setPeerRequest(peerRequestObj).build();

        return sendToConcordAndBuildHelenResponse(concordrequestObj);
    }

    /**
     * Parses the Protocol Buffer response from Concord and converts it into JSON. Method overridden because the
     * response for this API is of type JSONArray as opposed to JSONObject.
     *
     * @param concordResponse Protocol Buffer object containing Concord's reponse
     * @return Response in JSON format
     */
    @SuppressWarnings("unchecked")
    @Override
    protected JSONAware parseToJson(Concord.ConcordResponse concordResponse) {
        // Extract the peer response from the concord reponse envelope.
        Concord.PeerResponse peerResponse = concordResponse.getPeerResponse();

        // Read list of peer objects from the peer response object.
        List<Concord.Peer> peerList = peerResponse.getPeerList();
        JSONArray peerArr = new JSONArray();

        Map<String, String> rpcUrls = config.getRpcUrlsAsMap();

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

            // Store into a JSON array of all peers.
            peerArr.add(peerJson);
        }

        return peerArr;
    }
}

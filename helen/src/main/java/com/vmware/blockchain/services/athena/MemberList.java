/*
 * Copyright (c) 2018 VMware, Inc. All rights reserved. VMware Confidential
 */

package com.vmware.blockchain.services.athena;

import java.io.IOException;
import java.util.ArrayList;
import java.util.List;

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

import com.vmware.athena.Athena;
import com.vmware.blockchain.common.AthenaProperties;
import com.vmware.blockchain.connections.AthenaConnectionPool;
import com.vmware.blockchain.services.BaseServlet;

/**
 * Servlet class.
 */
@Controller
public final class MemberList extends BaseServlet {
    private static final long serialVersionUID = 1L;
    private static final Logger logger = LogManager.getLogger(MemberList.class);

    @Autowired
    public MemberList(AthenaProperties config, AthenaConnectionPool athenaConnectionPool) {
        super(config, athenaConnectionPool);

    }

    /**
     * Services a get request. Constructs a protobuf request of type peer request (enveloped in an athena request) as
     * defined in athena.proto. Sends this request to Athena. Parses the response and converts it into json for
     * responding to the client.
     *
     * @param request The request received by the servlet
     * @param response The response object used to respond to the client
     * @throws IOException
     */
    @RequestMapping(method = RequestMethod.GET, path = "/api/athena/members")
    public ResponseEntity<JSONAware> doGet() {
        // Construct a peer request object. Set its return_peers field.
        final Athena.PeerRequest peerRequestObj = Athena.PeerRequest.newBuilder().setReturnPeers(true).build();

        // Envelope the peer request object into an athena object.
        final Athena.AthenaRequest athenarequestObj =
                Athena.AthenaRequest.newBuilder().setPeerRequest(peerRequestObj).build();

        return sendToAthenaAndBuildHelenResponse(athenarequestObj);
    }

    /**
     * Parses the Protocol Buffer response from Athena and converts it into JSON. Method overridden because the response
     * for this API is of type JSONArray as opposed to JSONObject.
     *
     * @param athenaResponse Protocol Buffer object containing Athena's reponse
     * @return Response in JSON format
     */
    @SuppressWarnings("unchecked")
    @Override
    protected JSONAware parseToJSON(Athena.AthenaResponse athenaResponse) {
        // Extract the peer response from the athena reponse envelope.
        Athena.PeerResponse peerResponse = athenaResponse.getPeerResponse();

        // Read list of peer objects from the peer response object.
        List<Athena.Peer> peerList = new ArrayList<>();
        peerList = peerResponse.getPeerList();

        JSONArray peerArr = new JSONArray();

        // Iterate through each peer and construct
        // a corresponding JSON object
        for (Athena.Peer peer : peerList) {
            JSONObject peerJson = new JSONObject();
            peerJson.put("hostname", peer.getHostname());
            peerJson.put("address", peer.getAddress());
            peerJson.put("status", peer.getStatus());
            peerJson.put("millis_since_last_message", peer.getMillisSinceLastMessage());
            peerJson.put("millis_since_last_message_threshold", peer.getMillisSinceLastMessageThreshold());

            // Store into a JSON array of all peers.
            peerArr.add(peerJson);
        }

        return peerArr;
    }
}

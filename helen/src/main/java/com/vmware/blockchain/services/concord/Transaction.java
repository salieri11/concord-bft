/*
 * Copyright (c) 2018-2019 VMware, Inc. All rights reserved. VMware Confidential
 */

package com.vmware.blockchain.services.concord;

import java.util.Optional;
import java.util.UUID;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.json.simple.JSONAware;
import org.json.simple.JSONObject;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.security.access.prepost.PreAuthorize;
import org.springframework.stereotype.Controller;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestMethod;

import com.google.protobuf.ByteString;
import com.vmware.blockchain.connections.ConnectionPoolManager;
import com.vmware.blockchain.services.ConcordServlet;
import com.vmware.blockchain.services.ethereum.ApiHelper;
import com.vmware.blockchain.services.profiles.DefaultProfiles;
import com.vmware.concord.Concord;

/**
 * Servlet class.
 */
@Controller
public class Transaction extends ConcordServlet {
    private static final long serialVersionUID = 1L;
    private static final Logger logger = LogManager.getLogger(Transaction.class);

    @Autowired
    public Transaction(ConnectionPoolManager connectionPoolManager, DefaultProfiles defaultProfiles) {
        super(connectionPoolManager, defaultProfiles);
    }

    protected static JSONObject buildTransactionResponseJson(Concord.TransactionResponse tr) {
        // Construct the reponse JSON object.
        JSONObject responseJson = new JSONObject();

        responseJson.put("hash", ApiHelper.binaryStringToHex(tr.getHash()));
        responseJson.put("from", ApiHelper.binaryStringToHex(tr.getFrom()));
        responseJson.put("block_hash", ApiHelper.binaryStringToHex(tr.getBlockHash()));
        responseJson.put("block_number", tr.getBlockNumber());

        if (tr.hasTo()) {
            responseJson.put("to", ApiHelper.binaryStringToHex(tr.getTo()));
        }

        if (tr.hasContractAddress()) {
            responseJson.put("contract_address", ApiHelper.binaryStringToHex(tr.getContractAddress()));
        }

        if (tr.hasValue()) {
            responseJson.put("value", ApiHelper.binaryStringToHex(tr.getValue(), true));
        }

        if (tr.hasInput()) {
            responseJson.put("input", ApiHelper.binaryStringToHex(tr.getInput()));
        }
        responseJson.put("nonce", tr.getNonce());

        return responseJson;
    }

    /**
     * Services a get request. Constructs a protobuf request of type transaction request (enveloped in an concord
     * request) as defined in concord.proto. Sends this request to Concord. Parses the response and converts it into
     * json for responding to the client.
     *
     * @param request The request received by the servlet
     * @param response The response object used to respond to the client
     */
    @RequestMapping(path = {"/api/concord/transactions/{hash}", "/api/blockchains/{id}/concord/transactions/{hash}"},
            method = RequestMethod.GET)
    @PreAuthorize("@authHelper.canAccessChain(#id)")
    protected ResponseEntity<JSONAware> doGet(@PathVariable(name = "id", required = false) Optional<UUID> id,
            @PathVariable(value = "hash", required = true) String hash) {
        ResponseEntity<JSONAware> responseEntity;
        ByteString hashBytes = null;
        try {
            hashBytes = ApiHelper.hexStringToBinary(hash);
        } catch (Exception e) {
            logger.error("Invalid Hash");
            return new ResponseEntity<>(new JSONObject(), HttpStatus.BAD_REQUEST);
        }

        if (hashBytes == null) {
            logger.error("Invalid hash in request");
            return new ResponseEntity<>(new JSONObject(), HttpStatus.BAD_REQUEST);
        }

        // Construct a transaction request object.
        final Concord.TransactionRequest txRequestObj =
                Concord.TransactionRequest.newBuilder().setHash(hashBytes).build();

        // Envelope the transaction request object into an concord object.
        final Concord.ConcordRequest concordrequestObj =
                Concord.ConcordRequest.newBuilder().setTransactionRequest(txRequestObj).build();

        return getHelper(id).sendToConcordAndBuildHelenResponse(concordrequestObj);
    }

    /**
     * Parses the Protocol Buffer response from Concord and converts it into JSON.
     *
     * @param concordResponse Protocol Buffer object containing Concord's reponse
     * @return Response in JSON format
     */
    @SuppressWarnings("unchecked")
    @Override
    public JSONObject parseToJson(Concord.ConcordResponse concordResponse) {

        // Extract the transaction response from
        // the concord reponse envelope.
        Concord.TransactionResponse txResponse = concordResponse.getTransactionResponse();

        JSONObject responseJson = buildTransactionResponseJson(txResponse);

        // Concord EVM has status code '0' for success and other Positive
        // values to denote error. However, for JSON RPC '1' is success
        // and '0' is failure. Here we need to reverse status value of concord
        // response before returning it.
        responseJson.put("status", txResponse.getStatus() == 0 ? 1 : 0);

        return responseJson;
    }
}

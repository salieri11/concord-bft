/*
 * Copyright (c) 2018 VMware, Inc. All rights reserved. VMware Confidential
 */

package com.vmware.blockchain.services.concord;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.json.simple.JSONArray;
import org.json.simple.JSONAware;
import org.json.simple.JSONObject;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.stereotype.Controller;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestMethod;
import org.springframework.web.bind.annotation.RequestParam;

import com.vmware.blockchain.common.ConcordProperties;
import com.vmware.blockchain.common.Constants;
import com.vmware.blockchain.connections.ConcordConnectionPool;
import com.vmware.blockchain.services.BaseServlet;
import com.vmware.blockchain.services.ethereum.ApiHelper;
import com.vmware.concord.Concord;

/**
 * Controller to get transaction lists.
 */
@Controller
public class TransactionList extends BaseServlet {

    private static final long serialVersionUID = 1L;
    private static final Logger logger = LogManager.getLogger(TransactionList.class);
    private final String transactionListEndpoint = Constants.TRANSACTION_LIST_ENDPOINT;

    @Autowired
    public TransactionList(ConcordProperties config, ConcordConnectionPool concordConnectionPool) {
        super(config, concordConnectionPool);
    }

    /**
     * Services a get request. Constructs a protobuf request of type Transactionlist request (enveloped in an concord
     * request) as defined in concord.proto. Sends this request to Concord. Parses the response and converts it into
     * json for responding to the client.
     *
     */
    @RequestMapping(path = "/api/concord/transactions", method = RequestMethod.GET)
    public ResponseEntity<JSONAware> doGet(
            @RequestParam(name = "latest", defaultValue = "", required = false) String latestHash,
            @RequestParam(name = "count", required = false, defaultValue = "-1") long count) {
        if (count == -1) {
            count = Constants.TRANSACTIONLIST_DEFAULTCOUNT;
        }
        Concord.ConcordRequest concordRequest;

        try {
            Concord.TransactionListRequest.Builder txListReqBuilder = Concord.TransactionListRequest.newBuilder();

            if (!latestHash.isEmpty()) {
                txListReqBuilder.setLatest(ApiHelper.hexStringToBinary(latestHash));
            }
            txListReqBuilder.setCount(count);
            logger.info("requested count: " + count);

            concordRequest =
                    Concord.ConcordRequest.newBuilder().setTransactionListRequest(txListReqBuilder.build()).build();

            return sendToConcordAndBuildHelenResponse(concordRequest);

        } catch (Exception e) {
            logger.warn("Exception in transaction list", e);
            return new ResponseEntity<>(ApiHelper.errorJson(e.getMessage()), standardHeaders, HttpStatus.BAD_REQUEST);
        }
    }

    /**
     * Parses the Protocol Buffer response from Concord and converts it into JSON.
     *
     * @param concordResponse Protocol Buffer object containing Concord's reponse
     * @return Response in JSON format
     */
    @SuppressWarnings("unchecked")
    @Override
    protected JSONAware parseToJson(Concord.ConcordResponse concordResponse) {
        // Extract the transaction response from
        // the concord reponse envelope.
        Concord.TransactionListResponse txListResponse = concordResponse.getTransactionListResponse();

        // Construct the reponse JSON object.
        JSONObject responseJson = new JSONObject();
        JSONArray trArray = new JSONArray();
        for (Concord.TransactionResponse tr : txListResponse.getTransactionList()) {
            JSONObject trJson = Transaction.buildTransactionResponseJson(tr);
            trJson.put("url", transactionListEndpoint + "/" + ApiHelper.binaryStringToHex(tr.getHash()));
            trArray.add(trJson);
        }

        responseJson.put("transactions", trArray);
        if (txListResponse.hasNext()) {
            responseJson.put("next",
                    transactionListEndpoint + "?latest=" + ApiHelper.binaryStringToHex(txListResponse.getNext()));
        }
        return responseJson;
    }
}

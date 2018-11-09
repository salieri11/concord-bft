/*
 * Copyright (c) 2018 VMware, Inc. All rights reserved. VMware Confidential
 */

package com.vmware.blockchain.services.athena;

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

import com.vmware.athena.Athena;
import com.vmware.blockchain.common.AthenaProperties;
import com.vmware.blockchain.connections.AthenaConnectionPool;
import com.vmware.blockchain.services.BaseServlet;
import com.vmware.blockchain.services.ethereum.ApiHelper;

/**
 * Controller to get transaction lists.
 */
@Controller
public class TransactionList extends BaseServlet {

    private static final long serialVersionUID = 1L;
    private static final Logger logger = LogManager.getLogger(TransactionList.class);
    private final String transactionListEndpoint = config.getTransactionList_Endpoint();

    @Autowired
    public TransactionList(AthenaProperties config, AthenaConnectionPool athenaConnectionPool) {
        super(config, athenaConnectionPool);
    }

    /**
     * Services a get request. Constructs a protobuf request of type Transactionlist request (enveloped in an athena
     * request) as defined in athena.proto. Sends this request to Athena. Parses the response and converts it into json
     * for responding to the client.
     *
     */
    @RequestMapping(path = "/api/athena/transactions", method = RequestMethod.GET)
    public ResponseEntity<JSONAware> doGet(
            @RequestParam(name = "latest", defaultValue = "", required = false) String latestHash,
            @RequestParam(name = "count", required = false, defaultValue = "-1") long count) {
        if (count == -1) {
            count = config.getTransactionList_DefaultCount();
        }
        Athena.AthenaRequest athenaRequest;

        try {
            Athena.TransactionListRequest.Builder txListReqBuilder = Athena.TransactionListRequest.newBuilder();

            if (!latestHash.isEmpty()) {
                txListReqBuilder.setLatest(ApiHelper.hexStringToBinary(latestHash));
            }
            txListReqBuilder.setCount(count);
            logger.info("requested count: " + count);

            athenaRequest =
                    Athena.AthenaRequest.newBuilder().setTransactionListRequest(txListReqBuilder.build()).build();

            return sendToAthenaAndBuildHelenResponse(athenaRequest);

        } catch (Exception e) {
            logger.warn("Exception in transaction list", e);
            return new ResponseEntity<>(ApiHelper.errorJson(e.getMessage()), standardHeaders, HttpStatus.BAD_REQUEST);
        }
    }

    /**
     * Parses the Protocol Buffer response from Athena and converts it into JSON.
     *
     * @param athenaResponse Protocol Buffer object containing Athena's reponse
     * @return Response in JSON format
     */
    @SuppressWarnings("unchecked")
    @Override
    protected JSONAware parseToJson(Athena.AthenaResponse athenaResponse) {
        // Extract the transaction response from
        // the athena reponse envelope.
        Athena.TransactionListResponse txListResponse = athenaResponse.getTransactionListResponse();

        // Construct the reponse JSON object.
        JSONObject responseJson = new JSONObject();
        JSONArray trArray = new JSONArray();
        for (Athena.TransactionResponse tr : txListResponse.getTransactionList()) {
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

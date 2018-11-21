/*
 * Copyright (c) 2018 VMware, Inc. All rights reserved. VMware Confidential
 */

package com.vmware.blockchain.services.concord;

import java.util.List;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.json.simple.JSONArray;
import org.json.simple.JSONAware;
import org.json.simple.JSONObject;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.stereotype.Controller;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestMethod;

import com.vmware.blockchain.common.ConcordProperties;
import com.vmware.blockchain.common.Constants;
import com.vmware.blockchain.connections.ConcordConnectionPool;
import com.vmware.blockchain.services.BaseServlet;
import com.vmware.blockchain.services.ethereum.ApiHelper;
import com.vmware.concord.Concord;

/**
 * Servlet class.
 */
@Controller
public class BlockNumber extends BaseServlet {
    private static final long serialVersionUID = 1L;
    private Logger logger = LogManager.getLogger(BlockNumber.class);


    @Autowired
    public BlockNumber(ConcordProperties config, ConcordConnectionPool concordConnectionPool) {
        super(config, concordConnectionPool);
    }

    /**
     * Services a get request. Constructs a protobuf request of type blocknumber request (enveloped in an concord
     * request) as defined in concord.proto. Sends this request to Concord. Parses the response and converts it into
     * json for sending to client
     *
     * @param block The block number or block hash
     */
    @RequestMapping(method = RequestMethod.GET, path = "/api/concord/blocks/{block}")
    public ResponseEntity<JSONAware> getBlock(@PathVariable("block") String block) {
        // Block can either be a block number or block hash
        // Read the requested block number from the uri
        try {
            final Concord.BlockRequest blockRequestObj;
            // check if param is a number or hash
            if (block.chars().allMatch(Character::isDigit)) {
                Long number;
                number = Long.parseLong(block);
                blockRequestObj = Concord.BlockRequest.newBuilder().setNumber(number).build();
            } else {
                blockRequestObj = Concord.BlockRequest.newBuilder().setHash(ApiHelper.hexStringToBinary(block)).build();
            }

            // Envelope the blockRequest object into an concord object.
            final Concord.ConcordRequest concordRequestObj =
                    Concord.ConcordRequest.newBuilder().setBlockRequest(blockRequestObj).build();
            return sendToConcordAndBuildHelenResponse(concordRequestObj);

        } catch (Exception e) {
            logger.error("Invalid block number or hash");
            return new ResponseEntity<>(ApiHelper.errorJson("Invalid block number or hash"), standardHeaders,
                    HttpStatus.BAD_REQUEST);
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
    protected JSONObject parseToJson(Concord.ConcordResponse concordResponse) {

        // Extract the blocknumber response
        // from the concord reponse envelope.
        Concord.BlockResponse blockResponse = concordResponse.getBlockResponse();

        JSONArray transactionArr = new JSONArray();

        List<Concord.TransactionResponse> list = blockResponse.getTransactionList();

        for (Concord.TransactionResponse t : list) {
            String hash = ApiHelper.binaryStringToHex(t.getHash());
            JSONObject txJson = new JSONObject();
            txJson.put("hash", hash);
            txJson.put("url", Constants.TRANSACTION_URLPREFIX + hash);
            transactionArr.add(txJson);
        }

        JSONObject blockObj = new JSONObject();
        blockObj.put("transactions", transactionArr);

        blockObj.put("number", blockResponse.getNumber());

        String hash = ApiHelper.binaryStringToHex(blockResponse.getHash());
        String parentHash = ApiHelper.binaryStringToHex(blockResponse.getParentHash());

        blockObj.put("hash", hash);
        blockObj.put("parentHash", parentHash);
        blockObj.put("nonce", ApiHelper.binaryStringToHex(blockResponse.getNonce()));
        blockObj.put("size", blockResponse.getSize());

        if (blockResponse.hasTimestamp()) {
            blockObj.put("timestamp", blockResponse.getTimestamp());
        }

        return blockObj;
    }
}

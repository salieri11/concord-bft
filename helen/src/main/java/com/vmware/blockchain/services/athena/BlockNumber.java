/*
 * Copyright (c) 2018 VMware, Inc. All rights reserved. VMware Confidential
 */

package com.vmware.blockchain.services.athena;

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

import com.vmware.athena.Athena;
import com.vmware.blockchain.common.AthenaProperties;
import com.vmware.blockchain.connections.AthenaConnectionPool;
import com.vmware.blockchain.services.BaseServlet;
import com.vmware.blockchain.services.ethereum.ApiHelper;

/**
 * Servlet class.
 */
@Controller
public class BlockNumber extends BaseServlet {
    private static final long serialVersionUID = 1L;
    private Logger logger = LogManager.getLogger(BlockNumber.class);


    @Autowired
    public BlockNumber(AthenaProperties config, AthenaConnectionPool athenaConnectionPool) {
        super(config, athenaConnectionPool);
    }

    /**
     * Services a get request. Constructs a protobuf request of type blocknumber request (enveloped in an athena
     * request) as defined in athena.proto. Sends this request to Athena. Parses the response and converts it into json
     * for sending to client
     *
     * @param block The block number or block hash
     */
    @RequestMapping(method = RequestMethod.GET, path = "/api/athena/blocks/{block}")
    public ResponseEntity<JSONAware> getBlock(@PathVariable("block") String block) {
        // Block can either be a block number or block hash
        // Read the requested block number from the uri
        try {
            final Athena.BlockRequest blockRequestObj;
            // check if param is a number or hash
            if (block.chars().allMatch(Character::isDigit)) {
                Long number;
                number = Long.parseLong(block);
                blockRequestObj = Athena.BlockRequest.newBuilder().setNumber(number).build();
            } else {
                blockRequestObj = Athena.BlockRequest.newBuilder().setHash(ApiHelper.hexStringToBinary(block)).build();
            }

            // Envelope the blockRequest object into an athena object.
            final Athena.AthenaRequest athenaRequestObj =
                    Athena.AthenaRequest.newBuilder().setBlockRequest(blockRequestObj).build();
            return sendToAthenaAndBuildHelenResponse(athenaRequestObj);

        } catch (Exception e) {
            logger.error("Invalid block number or hash");
            return new ResponseEntity<>(ApiHelper.errorJson("Invalid block number or hash"), standardHeaders,
                    HttpStatus.BAD_REQUEST);
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
    protected JSONObject parseToJson(Athena.AthenaResponse athenaResponse) {

        // Extract the blocknumber response
        // from the athena reponse envelope.
        Athena.BlockResponse blockResponse = athenaResponse.getBlockResponse();

        JSONArray transactionArr = new JSONArray();

        List<Athena.TransactionResponse> list = blockResponse.getTransactionList();

        for (Athena.TransactionResponse t : list) {
            String hash = ApiHelper.binaryStringToHex(t.getHash());
            JSONObject txJson = new JSONObject();
            txJson.put("hash", hash);
            txJson.put("url", config.getTransaction_URLPrefix() + hash);
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

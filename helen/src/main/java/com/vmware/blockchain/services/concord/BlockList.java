/*
 * Copyright (c) 2018-2019 VMware, Inc. All rights reserved. VMware Confidential
 */

package com.vmware.blockchain.services.concord;

import java.util.ArrayList;
import java.util.List;
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

import com.vmware.blockchain.common.Constants;
import com.vmware.blockchain.connections.ConnectionPoolManager;
import com.vmware.blockchain.services.ConcordControllerHelper;
import com.vmware.blockchain.services.ConcordServlet;
import com.vmware.blockchain.services.ethereum.ApiHelper;
import com.vmware.blockchain.services.profiles.DefaultProfiles;
import com.vmware.concord.Concord;

/**
 * Servlet class.
 */
@Controller
public class BlockList extends ConcordServlet {
    private static final long serialVersionUID = 1L;
    private Logger logger = LogManager.getLogger(BlockList.class);

    @Autowired
    public BlockList(ConnectionPoolManager connectionPoolManager, DefaultProfiles defaultProfiles) {
        super(connectionPoolManager, defaultProfiles);
    }

    /**
     * Services a get request. Constructs a protobuf request of type blocklist request (enveloped in an concord request)
     * as defined in concord.proto. Sends this request to Concord. Parses the response and converts it into json for
     * responding to the client.
     *
     * @param latest The block from which to start the list
     * @param count Number of blocks expected
     */
    // ** - tells spring to match anything in path
    @RequestMapping(method = RequestMethod.GET, path = {"/api/concord/blocks", "/api/blockchains/{id}/concord/blocks"})
    public ResponseEntity<JSONAware> getBlockList(
            @PathVariable(name = "id", required = false) Optional<UUID> id,
            @RequestParam(name = "latest", defaultValue = "-1", required = false) long latest,
            @RequestParam(name = "count", required = false, defaultValue = "-1") long count) {

        // get the helper
        final ConcordControllerHelper helper = getHelper(id);
        // Construct a blocksListRequest object.
        Concord.BlockListRequest.Builder b = Concord.BlockListRequest.newBuilder();
        // If end is null, Concord assumes end is the latest block
        if (latest != -1) {
            b.setLatest(latest);
        }

        // If listLength is null, request for default no. of blocks
        if (count == -1) {
            count = Constants.BLOCKLIST_DEFAULTCOUNT;
        }
        b.setCount(count);

        Concord.BlockListRequest blocksListRequestObj = b.build();

        // Envelope the blocksListRequest object into an concord object.
        final Concord.ConcordRequest concordrequestObj =
                Concord.ConcordRequest.newBuilder().setBlockListRequest(blocksListRequestObj).build();

        return helper.sendToConcordAndBuildHelenResponse(concordrequestObj);
    }

    /**
     * Parses the Protocol Buffer response from Concord and converts it into JSON.
     *
     * @param concordResponse Protocol Buffer object containing Concord's reponse
     * @return Response in JSON format
     */
    public JSONAware parseToJson(Concord.ConcordResponse concordResponse) {
        try {
            // Extract the blocklist response
            // from the concord reponse envelope.
            Concord.BlockListResponse blockListResponse = concordResponse.getBlockListResponse();

            long earliestBlock = -1L;

            // Read list of blocks from the blocks list response object.
            List<Concord.BlockBrief> blockList = new ArrayList<>();
            blockList = blockListResponse.getBlockList();

            JSONArray blockArr = new JSONArray();

            // Iterate through each block and construct a corresponding JSON object
            for (Concord.BlockBrief block : blockList) {
                JSONObject blockJson = new JSONObject();

                long number = block.getNumber();
                String hexString = ApiHelper.binaryStringToHex(block.getHash());

                blockJson.put("number", number);
                blockJson.put("hash", hexString);

                String url = Constants.BLOCKLIST_URLPREFIX + number;

                blockJson.put("url", url);

                // Store into a JSON array of all blocks.
                blockArr.add(blockJson);
                earliestBlock = number;
            }

            // Construct the reponse JSON object.
            JSONObject responseJson = new JSONObject();
            responseJson.put("blocks", blockArr);
            if (earliestBlock > 0) {
                responseJson.put("next", Constants.BLOCKLIST_NEXTPREFIX + (earliestBlock - 1));
            }

            return responseJson;
        } catch (Exception e) {
            logger.error("parseToJson", e);
            return null;
        }
    }
}

/*
 * Copyright (c) 2019 VMware, Inc. All rights reserved. VMware Confidential
 */

package com.vmware.blockchain.services;

import java.util.UUID;

import org.apache.logging.log4j.LogManager;
import org.json.simple.JSONAware;
import org.json.simple.JSONObject;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;

import com.vmware.blockchain.common.ConcordConnectionException;
import com.vmware.blockchain.common.ErrorCodeType;
import com.vmware.blockchain.connections.ConcordConnectionPool;
import com.vmware.blockchain.services.ethereum.ApiHelper;
import com.vmware.concord.Concord;
import com.vmware.concord.ConcordHelper;
import com.vmware.concord.IConcordConnection;

import lombok.Getter;

/**
 * Concord Communication Helper.  Given a connection and a ParseToJson object, send to the concord instance.
 */
public class ConcordControllerHelper {

    @Getter
    private UUID blockchain;
    private ConcordConnectionPool concordConnectionPool;
    private IParseToJson parseToJson;

    /**
     * Helper functions to talk to concord.
     */
    public ConcordControllerHelper(UUID blockchain, ConcordConnectionPool concordConnectionPool,
            IParseToJson parseToJson) {
        this.blockchain = blockchain;
        this.concordConnectionPool = concordConnectionPool;
        this.parseToJson = parseToJson;
    }

    private Concord.ConcordResponse forwardToConcord(Concord.ConcordRequest req) throws ConcordConnectionException {
        IConcordConnection conn = null;
        Concord.ConcordResponse concordResponse;
        try {
            conn = concordConnectionPool.getConnection();
            if (conn == null) {
                throw new ConcordConnectionException(ErrorCodeType.CONCORD_CONNECTION);
            }
            boolean res = ConcordHelper.sendToConcord(req, conn);
            if (!res) {
                throw new ConcordConnectionException(ErrorCodeType.CONCORD_SEND_FAILED);
            }

            // receive response from Concord
            concordResponse = ConcordHelper.receiveFromConcord(conn);
            if (concordResponse == null) {
                throw new ConcordConnectionException(ErrorCodeType.CONCORD_INVALID_RESPONSE);
            }
            return concordResponse;
        } catch (Exception e) {
            throw new ConcordConnectionException(ErrorCodeType.CONCORD_INTERNAL_ERROR + e.getMessage());
        } finally {
            concordConnectionPool.putConnection(conn);
        }
    }

    private ResponseEntity<JSONAware> buildHelenResponse(Concord.ConcordResponse concordResponse) {
        JSONAware respObject;
        HttpStatus status;
        if (concordResponse.getErrorResponseCount() == 0) {
            respObject = parseToJson.parseToJson(blockchain, concordResponse);
            status = respObject == null ? HttpStatus.INTERNAL_SERVER_ERROR : HttpStatus.OK;
        } else {
            Concord.ErrorResponse errorResp = concordResponse.getErrorResponse(0);

            String message = errorResp.getDescription();
            JSONObject obj = new JSONObject();
            obj.put("error", message);
            respObject = obj;
            // trying to be a little fancy with status codes here, but we should
            // probably change ErrorResponse to include a "code" field, so Concord
            // can signal exactly what kind of error happened
            if (message.contains("not found")) {
                // block/transaction not found
                status = HttpStatus.NOT_FOUND;
            } else if (message.contains("Missing") || message.contains("request")) {
                // Missing required parameter
                // Invalid ... request
                status = HttpStatus.BAD_REQUEST;
            } else {
                status = HttpStatus.INTERNAL_SERVER_ERROR;
            }
        }
        return new ResponseEntity<>(respObject, status);
    }

    /**
     * Send the request to concord, and process the response for helen.
     */
    public ResponseEntity<JSONAware> sendToConcordAndBuildHelenResponse(Concord.ConcordRequest concordRequest) {
        try {
            Concord.ConcordResponse concordResponse = forwardToConcord(concordRequest);
            return buildHelenResponse(concordResponse);
        } catch (ConcordConnectionException ace) {
            LogManager.getLogger(ConcordServlet.class).warn("Concord Exception: ", ace);
            return new ResponseEntity<>(ApiHelper.errorJson(ace.getMessage()), HttpStatus.INTERNAL_SERVER_ERROR);
        }
    }

}

/*
 * Copyright (c) 2018-2019 VMware, Inc. All rights reserved. VMware Confidential
 */

package com.vmware.blockchain.services;

import org.apache.logging.log4j.LogManager;
import org.json.simple.JSONAware;
import org.json.simple.JSONObject;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.HttpHeaders;
import org.springframework.http.HttpStatus;
import org.springframework.http.MediaType;
import org.springframework.http.ResponseEntity;
import org.springframework.stereotype.Controller;

import com.vmware.blockchain.common.ConcordConnectionException;
import com.vmware.blockchain.common.ConcordProperties;
import com.vmware.blockchain.connections.ConcordConnectionPool;
import com.vmware.blockchain.services.ethereum.ApiHelper;

import com.vmware.concord.Concord;
import com.vmware.concord.ConcordHelper;
import com.vmware.concord.IConcordConnection;

/**
 * Base for all Helen Controllers.
 */
@Controller
public abstract class BaseServlet {
    protected static final long serialVersionUID = 1L;

    protected HttpHeaders standardHeaders;
    protected ConcordConnectionPool concordConnectionPool;
    protected ConcordProperties config;

    @Autowired
    protected BaseServlet(ConcordProperties config, ConcordConnectionPool concordConnectionPool) {
        this.concordConnectionPool = concordConnectionPool;
        this.config = config;
        standardHeaders = new HttpHeaders();
        standardHeaders.setContentType(MediaType.APPLICATION_JSON_UTF8);
        standardHeaders.set("Content-Transfer-Encoding", "8BIT");
    }

    protected abstract JSONAware parseToJson(Concord.ConcordResponse concordResponse);

    /**
     * Process get request.
     *
     * @param req - Concord request object
     */
    protected Concord.ConcordResponse forwardToConcord(Concord.ConcordRequest req) throws ConcordConnectionException {
        IConcordConnection conn = null;
        Concord.ConcordResponse concordResponse;
        try {
            conn = concordConnectionPool.getConnection();
            if (conn == null) {
                throw new ConcordConnectionException("Unable to get concord " + "connection");
            }

            boolean res = ConcordHelper.sendToConcord(req, conn);
            if (!res) {
                throw new ConcordConnectionException("Sending to concord failed");
            }

            // receive response from Concord
            concordResponse = ConcordHelper.receiveFromConcord(conn);
            if (concordResponse == null) {
                throw new ConcordConnectionException("Concord sent invalid response");
            }
            return concordResponse;
        } catch (Exception e) {
            throw new ConcordConnectionException("Concord internal error: " + e.getMessage());
        } finally {
            concordConnectionPool.putConnection(conn);
        }
    }

    protected ResponseEntity<JSONAware> buildHelenResponse(Concord.ConcordResponse concordResponse) {
        JSONAware respObject;
        HttpStatus status;
        if (concordResponse.getErrorResponseCount() == 0) {
            respObject = parseToJson(concordResponse);
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
        return new ResponseEntity<>(respObject, standardHeaders, status);
    }

    protected ResponseEntity<JSONAware> sendToConcordAndBuildHelenResponse(Concord.ConcordRequest concordRequest) {
        try {
            Concord.ConcordResponse concordResponse = forwardToConcord(concordRequest);
            return buildHelenResponse(concordResponse);
        } catch (ConcordConnectionException ace) {
            LogManager.getLogger(BaseServlet.class).warn("Concord Exception: ", ace);
            return new ResponseEntity<>(ApiHelper.errorJson(ace.getMessage()), standardHeaders,
                    HttpStatus.INTERNAL_SERVER_ERROR);
        }
    }
}

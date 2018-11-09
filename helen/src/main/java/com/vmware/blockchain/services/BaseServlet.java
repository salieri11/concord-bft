/*
 * Copyright (c) 2018 VMware, Inc. All rights reserved. VMware Confidential
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

import com.vmware.athena.Athena;
import com.vmware.blockchain.common.AthenaConnectionException;
import com.vmware.blockchain.common.AthenaProperties;
import com.vmware.blockchain.connections.AthenaConnectionPool;
import com.vmware.blockchain.connections.IAthenaConnection;
import com.vmware.blockchain.services.ethereum.ApiHelper;
import com.vmware.blockchain.services.ethereum.AthenaHelper;

/**
 * Base for all Helen Controllers.
 */
@Controller
public abstract class BaseServlet {
    protected static final long serialVersionUID = 1L;

    protected AthenaProperties config;
    protected HttpHeaders standardHeaders;
    protected AthenaConnectionPool athenaConnectionPool;

    @Autowired
    protected BaseServlet(AthenaProperties config, AthenaConnectionPool athenaConnectionPool) {
        this.config = config;
        this.athenaConnectionPool = athenaConnectionPool;
        standardHeaders = new HttpHeaders();
        standardHeaders.setContentType(MediaType.APPLICATION_JSON_UTF8);
        standardHeaders.set("Content-Transfer-Encoding", "8BIT");
    }

    protected abstract JSONAware parseToJson(Athena.AthenaResponse athenaResponse);

    /**
     * Process get request.
     *
     * @param req - Athena request object
     */
    protected Athena.AthenaResponse forwardToAthena(Athena.AthenaRequest req) throws AthenaConnectionException {
        IAthenaConnection conn = null;
        Athena.AthenaResponse athenaResponse;
        try {
            conn = athenaConnectionPool.getConnection();
            if (conn == null) {
                throw new AthenaConnectionException("Unable to get athena " + "connection");
            }

            boolean res = AthenaHelper.sendToAthena(req, conn, config);
            if (!res) {
                throw new AthenaConnectionException("Sending to athena failed");
            }

            // receive response from Athena
            athenaResponse = AthenaHelper.receiveFromAthena(conn);
            if (athenaResponse == null) {
                throw new AthenaConnectionException("Athena sent invalid response");
            }
            return athenaResponse;
        } catch (Exception e) {
            throw new AthenaConnectionException("Athena internal error: " + e.getMessage());
        } finally {
            athenaConnectionPool.putConnection(conn);
        }
    }

    protected ResponseEntity<JSONAware> buildHelenResponse(Athena.AthenaResponse athenaResponse) {
        JSONAware respObject;
        HttpStatus status;
        if (athenaResponse.getErrorResponseCount() == 0) {
            respObject = parseToJson(athenaResponse);
            status = respObject == null ? HttpStatus.INTERNAL_SERVER_ERROR : HttpStatus.OK;
        } else {
            Athena.ErrorResponse errorResp = athenaResponse.getErrorResponse(0);

            String message = errorResp.getDescription();
            JSONObject obj = new JSONObject();
            obj.put("error", message);
            respObject = obj;
            // trying to be a little fancy with status codes here, but we should
            // probably change ErrorResponse to include a "code" field, so Athena
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

    protected ResponseEntity<JSONAware> sendToAthenaAndBuildHelenResponse(Athena.AthenaRequest athenaRequest) {
        try {
            Athena.AthenaResponse athenaResponse = forwardToAthena(athenaRequest);
            return buildHelenResponse(athenaResponse);
        } catch (AthenaConnectionException ace) {
            LogManager.getLogger(BaseServlet.class).warn("Athena Exception: ", ace);
            return new ResponseEntity<>(ApiHelper.errorJson(ace.getMessage()), standardHeaders,
                    HttpStatus.INTERNAL_SERVER_ERROR);
        }
    }
}

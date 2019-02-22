/*
 * Copyright (c) 2019 VMware, Inc. All rights reserved. VMware Confidential
 */

package com.vmware.concord.agent;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.json.simple.JSONObject;
import org.json.simple.parser.JSONParser;
import org.json.simple.parser.ParseException;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.context.annotation.ComponentScan;
import org.springframework.http.HttpHeaders;
import org.springframework.http.MediaType;
import org.springframework.stereotype.Controller;


/**
 * AgentDispatcher -
 * Handles all the incoming requests to Concord-Agent.
 */
@Controller
@ComponentScan("com.vmware.concord.connections")
public final class AgentDispatcher {
    private static final long serialVersionUID = 1L;
    public static long netVersion;
    public static boolean netVersionSet;
    private static Logger logger = LogManager.getLogger("agentLogger");
    private String jsonRpc;
    private HttpHeaders standardHeaders;

    @Autowired
    public AgentDispatcher() throws ParseException {

        JSONParser p = new JSONParser();
        try {
            jsonRpc = Constants.JSONRPC;
        } catch (Exception e) {
            logger.error("Failed to read RPC information from config file", e);
        }

        standardHeaders = new HttpHeaders();
        standardHeaders.setContentType(MediaType.APPLICATION_JSON_UTF8);
    }

    /**
     * Constructs the response in case of error.
     *
     * @param message Error message
     * @param id Request Id
     * @param jsonRpc RPC version
     * @return Error message string
     */
    @SuppressWarnings("unchecked")
    public static JSONObject errorMessage(String message, long id, String jsonRpc) {
        JSONObject responseJson = new JSONObject();
        responseJson.put("id", id);
        responseJson.put("jsonprc", jsonRpc);

        JSONObject error = new JSONObject();
        error.put("message", message);
        responseJson.put("error", error);

        return responseJson;
    }
}

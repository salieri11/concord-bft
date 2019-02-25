/*
 * Copyright (c) 2019 VMware, Inc. All rights reserved. VMware Confidential
 */

package com.vmware.concord.agent;

import java.io.IOException;
import java.io.PrintWriter;
import java.io.StringWriter;
import java.util.stream.Collectors;

import javax.servlet.http.HttpServletRequest;

import org.json.simple.JSONObject;

/**
 * Helper functions for Agent APIs.
 */
public class ApiHelper {

    /**
     * An utility function to convert java exceptions stack trace into Strings.
     *
     * @return string of exception stack trace
     */
    public static String exceptionToString(Exception e) {
        StringWriter sw = new StringWriter();
        PrintWriter pw = new PrintWriter(sw);
        e.printStackTrace(pw);
        return sw.toString(); // stack trace as a string
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
    public static String errorMessage(String message, long id, String jsonRpc) {
        JSONObject responseJson = new JSONObject();
        responseJson.put("id", id);
        responseJson.put("jsonprc", jsonRpc);

        JSONObject error = new JSONObject();
        error.put("message", message);
        responseJson.put("error", error);

        return responseJson.toJSONString();
    }

    /**
     * Prepare a json error message.
     */
    public static JSONObject errorJson(String message) {
        JSONObject error = new JSONObject();
        error.put("error", message);
        return error;
    }

    public static String getRequestBody(final HttpServletRequest request) throws IOException {
        String paramString = request.getReader().lines().collect(Collectors.joining(System.lineSeparator()));
        return paramString;
    }
}

/*
 * Copyright (c) 2019 VMware, Inc. All rights reserved. VMware Confidential
 */

package com.vmware.blockchain.common.csp.api.response;

import java.util.List;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.fasterxml.jackson.annotation.JsonInclude;
import com.fasterxml.jackson.annotation.JsonProperty;
import com.fasterxml.jackson.databind.annotation.JsonDeserialize;
import com.vmware.blockchain.common.csp.api.client.deserializer.CspStackTraceElementArrayJsonDeserializer;

import lombok.Data;

/**
 * Represents an error response from csp.
 */
@Data
@JsonInclude(JsonInclude.Include.NON_NULL)
@JsonIgnoreProperties(ignoreUnknown = true)
public class CspErrorResponse {

    @JsonProperty("message")
    private String message;
    @JsonProperty("errorCode")
    private String error;
    @JsonProperty("statusCode")
    private Integer statusCode;
    @JsonProperty("requestId")
    private String requestId;
    @JsonProperty("service")
    private String service;
    @JsonProperty("documentKind")
    private String documentKind;
    @JsonProperty("stackTrace")
    @JsonDeserialize(using = CspStackTraceElementArrayJsonDeserializer.class)
    private List<StackTraceElement> stackTrace;
    @JsonProperty("causes")
    private List<CspErrorCause> causes;

    /**
     * Class to hold causes for error returned by csp.
     */
    @Data
    @JsonInclude(JsonInclude.Include.NON_NULL)
    public static class CspErrorCause {

        @JsonProperty("message")
        private String message;
        @JsonProperty("cspError")
        private String cspError;
        @JsonProperty("statusCode")
        private Integer statusCode;
        @JsonProperty("requestId")
        private String requestId;
        @JsonProperty("service")
        private String service;
        @JsonProperty("stackTrace")
        @JsonDeserialize(using = CspStackTraceElementArrayJsonDeserializer.class)
        private List<StackTraceElement> stackTrace;
        @JsonProperty("causes")
        private List<CspErrorCause> causes;
        @JsonProperty("documentKind")
        private String documentKind;
    }
}
/*
 * Copyright (c) 2020 VMware, Inc. All rights reserved. VMware Confidential
 */

package com.vmware.blockchain.agent.services.exceptions;

import java.io.IOException;
import java.util.Arrays;
import java.util.Optional;

import org.springframework.http.HttpStatus;
import org.springframework.security.access.AccessDeniedException;
import org.springframework.web.bind.MethodArgumentNotValidException;
import org.springframework.web.bind.MissingServletRequestParameterException;

import com.fasterxml.jackson.databind.exc.InvalidFormatException;
import com.google.protobuf.InvalidProtocolBufferException;

import lombok.Getter;
import lombok.Setter;

/**
 * Agent error status codes.
 */
@Getter
public enum ErrorCode {
    //Agent specific
    CONFIGURATION_RETRIEVAL_FAILURE(HttpStatus.INTERNAL_SERVER_ERROR, "Cannot retrieve node configuration", null),
    NODE_START_FAILED(HttpStatus.INTERNAL_SERVER_ERROR, "Cannot start node", null),
    ERROR_POPULATING_NODE_CONFIG(HttpStatus.INTERNAL_SERVER_ERROR, "Cannot start node", null),
    DOCKER_CLIENT_PARSING_IMAGE_NAME_FAILURE(HttpStatus.INTERNAL_SERVER_ERROR,
                                             "Parsing image name failure, using default values", null),
    DOCKER_CLIENT_PULLING_IMAGE_FAILURE(HttpStatus.INTERNAL_SERVER_ERROR, "Pulling image failure", null),
    RETRY_REST_CALL_FAILURE(HttpStatus.INTERNAL_SERVER_ERROR, "Agent external call failure", InternalError.class),
    DAML_NODE_MISSING(HttpStatus.INTERNAL_SERVER_ERROR, "DAML Node type not provided", null),
    INVALID_BLOCKCHAIN_NETWORK_TYPE(HttpStatus.INTERNAL_SERVER_ERROR, "Invalid blockchain network type", null),
    //General
    NOT_FOUND(HttpStatus.NOT_FOUND, "Not found", null),
    NOTARY_SIGNATURE_VERIFICATION_FAILED(HttpStatus.INTERNAL_SERVER_ERROR,
                                         "Notary Signature Verification Failed", null),
    FORBIDDEN(HttpStatus.FORBIDDEN, "Forbidden", AccessDeniedException.class),
    WRONG_ARG_REQUEST(HttpStatus.BAD_REQUEST, "Bad request", IllegalArgumentException.class),
    PROTO_BUF_ERROR(HttpStatus.INTERNAL_SERVER_ERROR, "Internal server error",
                    InvalidProtocolBufferException.class),
    SERVER_ERROR(HttpStatus.INTERNAL_SERVER_ERROR, "Internal server error", IOException.class),
    MISSING_SERVLET_ARG(HttpStatus.BAD_REQUEST, "Bad request", MissingServletRequestParameterException.class),
    METHOD_ARG_NOT_VALID(HttpStatus.BAD_REQUEST, "Bad request", MethodArgumentNotValidException.class),
    METHOD_NOT_ALLOWED(HttpStatus.METHOD_NOT_ALLOWED, "Unsupported operation", UnsupportedOperationException.class),
    INVALID_FORMAT(HttpStatus.BAD_REQUEST, "Bad request", InvalidFormatException.class),
    //default
    UNKNOWN_ERROR(HttpStatus.INTERNAL_SERVER_ERROR, "Unknown error", null);

    private final HttpStatus httpStatus;
    private final String message;

    @Setter
    private Class<? extends Throwable> exceptionType;

    ErrorCode(HttpStatus httpStatus, String message, Class<? extends Throwable> clazz) {
        this.httpStatus = httpStatus;
        this.message = message;
        this.exceptionType = clazz;
    }

    static ErrorCode findByException(Throwable ex) {
        Optional<ErrorCode> error =
                Arrays.stream(ErrorCode.values()).filter(code -> code.getExceptionType() == ex.getClass()).findFirst();
        return error.orElseGet(() -> {
            return UNKNOWN_ERROR;
        });
    }
}

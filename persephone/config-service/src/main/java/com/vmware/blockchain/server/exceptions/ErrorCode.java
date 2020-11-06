/*
 * Copyright (c) 2020 VMware, Inc. All rights reserved. VMware Confidential
 */

package com.vmware.blockchain.server.exceptions;

import org.springframework.http.HttpStatus;

import io.grpc.Status;
import lombok.Getter;
import lombok.Setter;

/**
 * CS error status codes.
 */
@Getter
public enum ErrorCode {
    CONFIGURATION_GENERATION_FAILURE(HttpStatus.INTERNAL_SERVER_ERROR, Status.INTERNAL,
                                     "CS could not generate configuration.", null),
    CONCORD_CONFIGURATION_FAILURE(HttpStatus.INTERNAL_SERVER_ERROR, Status.INTERNAL,
                                     "CS could not generate Concord configuration.", null),
    BFT_CONFIGURATION_FAILURE(HttpStatus.INTERNAL_SERVER_ERROR, Status.INTERNAL,
                                     "CS could not generate BFT configuration.", null),
    CONFIGURATION_SERVICE_INPUT_MISSING_NODES(HttpStatus.BAD_REQUEST, Status.INVALID_ARGUMENT,
                                              "Unable to retrieve nodes from the request.", null),
    GENERATE_NODE_INDEPENDENT_CONFIG_FAILURE(HttpStatus.INTERNAL_SERVER_ERROR, Status.INTERNAL,
                                             "Failed to generate node configuration.", null),
    GENERATE_TLS_NODE_IDENTITIES_FAILURE(HttpStatus.INTERNAL_SERVER_ERROR, Status.INTERNAL,
                                         "Failed to generate TLS node identities.", null),
    CONVERT_TO_BFT_TLS_NODE_IDENTITIES_FAILURE(HttpStatus.INTERNAL_SERVER_ERROR, Status.INTERNAL,
                                               "Failed to convert to BFT TLS node identities.", null),
    GENERATE_NODE_CONFIG_INVALID_INPUT_FAILURE(HttpStatus.INTERNAL_SERVER_ERROR, Status.INVALID_ARGUMENT,
                                               "Failed to generate node configuration due to invalid input.",
                                               null),
    CONCORD_CONFIGURATION_NO_NODES_FAILURE(HttpStatus.INTERNAL_SERVER_ERROR, Status.INVALID_ARGUMENT,
                                           "No nodes available to generate configuration.", null),
    GENERATE_TLS_NODE_IDENTITIES_INVALID_INPUT_FAILURE(HttpStatus.INTERNAL_SERVER_ERROR, Status.INTERNAL,
                                                       "Failed to generate TLS node identities due to invalid "
                                                       + "data.", null),
    CONCORD_CONFIGURATION_INVALID_INPUT_FAILURE(HttpStatus.INTERNAL_SERVER_ERROR, Status.INVALID_ARGUMENT,
                                                "Failed to generate configuration due to invalid input.",
                                                null),
    CONCORD_CONFIGURATION_INVALID_REPLICA_SIZE_FAILURE(HttpStatus.INTERNAL_SERVER_ERROR, Status.INVALID_ARGUMENT,
                                                       "Failed to generate configuration due to invalid number"
                                                       + " of replicas.", null),
    CLIENT_CONFIG_INVALID_INPUT_FAILURE(HttpStatus.INTERNAL_SERVER_ERROR, Status.INVALID_ARGUMENT,
                                        "Failed to generate Client configuration due to invalid input.", null),
    CONCORD_CONFIGURATION_INVALID_PRINCIPALS(HttpStatus.INTERNAL_SERVER_ERROR, Status.INTERNAL,
                                             "Failed to read Concord principals file.", null),
    //default
    UNKNOWN_ERROR(HttpStatus.INTERNAL_SERVER_ERROR, Status.UNKNOWN, "Unknown error in Config Service",
                  null);

    private final HttpStatus httpStatus;
    private final Status grpcStatus;
    private final String message;

    @Setter
    private Class<? extends Throwable> exceptionType;

    ErrorCode(HttpStatus httpStatus, Status grpcStatus, String message, Class<? extends Throwable> clazz) {
        this.httpStatus = httpStatus;
        this.grpcStatus = grpcStatus;
        this.message = message;
        this.exceptionType = clazz;
    }
}

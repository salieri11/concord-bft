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
    //default
    UNKNOWN_ERROR(HttpStatus.INTERNAL_SERVER_ERROR, Status.UNKNOWN, "Unknown error in Config Service", null);

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

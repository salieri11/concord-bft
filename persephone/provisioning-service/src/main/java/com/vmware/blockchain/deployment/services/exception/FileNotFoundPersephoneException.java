/*
 * Copyright (c) 2020 VMware, Inc. All rights reserved. VMware Confidential
 */

package com.vmware.blockchain.deployment.services.exception;

import org.springframework.http.HttpStatus;

import io.grpc.Status;

/**
 * Indicates that a file is not found.
 */
public class FileNotFoundPersephoneException extends PersephoneException {

    public FileNotFoundPersephoneException(HttpStatus httpStatus, Throwable cause, String message, Object... args) {
        super(httpStatus, cause, message, args);
        this.setGrpcStatus(Status.FAILED_PRECONDITION);
    }

    public FileNotFoundPersephoneException(HttpStatus httpStatus, String message, Object... args) {
        super(httpStatus, message, args);
        this.setGrpcStatus(Status.FAILED_PRECONDITION);
    }

}

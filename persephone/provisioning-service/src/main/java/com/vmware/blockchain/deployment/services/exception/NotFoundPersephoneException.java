/*
 * Copyright (c) 2020 VMware, Inc. All rights reserved. VMware Confidential
 */

package com.vmware.blockchain.deployment.services.exception;

import org.springframework.http.HttpStatus;

import io.grpc.Status;

/**
 * Return not found, 404.
 */
public class NotFoundPersephoneException extends PersephoneException {

    private static final long serialVersionUID = 1L;

    public NotFoundPersephoneException(String message, Object... args) {
        super(HttpStatus.NOT_FOUND, message, args);
        this.setGrpcStatus(Status.NOT_FOUND);
    }

    public NotFoundPersephoneException(Throwable cause, String message, Object... args) {
        super(HttpStatus.NOT_FOUND, cause, message, args);
        this.setGrpcStatus(Status.NOT_FOUND);
    }

}

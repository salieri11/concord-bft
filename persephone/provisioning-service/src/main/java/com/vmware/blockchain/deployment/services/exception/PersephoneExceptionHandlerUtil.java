/*
 * Copyright (c) 2020 VMware, Inc. All rights reserved. VMware Confidential
 */

package com.vmware.blockchain.deployment.services.exception;

import java.io.IOException;
import java.util.Map;

import org.apache.commons.lang3.tuple.Pair;
import org.springframework.core.NestedExceptionUtils;
import org.springframework.http.HttpStatus;
import org.springframework.security.access.AccessDeniedException;
import org.springframework.web.bind.MethodArgumentNotValidException;
import org.springframework.web.bind.MissingServletRequestParameterException;

import com.fasterxml.jackson.databind.exc.InvalidFormatException;
import com.google.common.collect.ImmutableMap;
import com.google.protobuf.InvalidProtocolBufferException;

import io.grpc.Status;
import lombok.Value;

/**
 * Holds helper method for error description composing.
 */
public class PersephoneExceptionHandlerUtil {

    // Normally the status code is part of the PersephoneException.  There are a handful of exceptions that
    // we want to handle with specific status codes
    private static final Map<Class<? extends Throwable>, Pair<HttpStatus, Status>> statusCodes =
            new ImmutableMap.Builder<Class<? extends Throwable>, Pair<HttpStatus, Status>>()
                    .put(AccessDeniedException.class, Pair.of(HttpStatus.FORBIDDEN, Status.PERMISSION_DENIED))
                    .put(IllegalArgumentException.class, Pair.of(HttpStatus.BAD_REQUEST, Status.INVALID_ARGUMENT))
                    .put(InvalidProtocolBufferException.class,
                         Pair.of(HttpStatus.INTERNAL_SERVER_ERROR, Status.INTERNAL))
                    .put(IOException.class, Pair.of(HttpStatus.INTERNAL_SERVER_ERROR, Status.INTERNAL))
                    .put(MissingServletRequestParameterException.class,
                         Pair.of(HttpStatus.BAD_REQUEST, Status.UNAVAILABLE))
                    .put(MethodArgumentNotValidException.class,
                         Pair.of(HttpStatus.BAD_REQUEST, Status.INVALID_ARGUMENT))
                    .put(UnsupportedOperationException.class,
                         Pair.of(HttpStatus.METHOD_NOT_ALLOWED, Status.UNAVAILABLE))
                    .put(InvalidFormatException.class, Pair.of(HttpStatus.BAD_REQUEST, Status.INVALID_ARGUMENT))
                    .build();

    @Value
    static class ErrorMessage {

        String errorCode;
        String errorMessage;
        Pair<HttpStatus, Status> status;
        String path;
    }

    static ErrorMessage getErrorMessage(Throwable ex, String path) {
        path = path != null ? path : "";
        String errorCode = ex.getClass().getSimpleName();
        // Get the innermost problem (or the exception itself)
        Throwable root = NestedExceptionUtils.getMostSpecificCause(ex);
        // If the exception class is in the map, use that type
        Pair<HttpStatus, Status> status = statusCodes.getOrDefault(root.getClass(),
                                                                   Pair.of(HttpStatus.INTERNAL_SERVER_ERROR,
                                                                           Status.INTERNAL));
        // However, if this is a PersephoneException, pull the code from exception
        if (ex instanceof PersephoneException) {
            status = Pair.of(((PersephoneException) ex).getHttpStatus(), status.getValue());
        } else if (root instanceof InvalidFormatException && ((InvalidFormatException) root).getTargetType().isEnum()) {
            status = Pair.of(HttpStatus.BAD_REQUEST, Status.INVALID_ARGUMENT);
        }
        // For now, let's always print a stacktrace. This may change in the future.
        return new ErrorMessage(errorCode, ex.getMessage(), status, path);
    }

}

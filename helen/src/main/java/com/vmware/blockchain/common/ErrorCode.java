/*
 * Copyright (c) 2019 VMware, Inc. All rights reserved. VMware Confidential
 */

package com.vmware.blockchain.common;

/**
 * Error Messages. At some point we will turn this into a class loading from a resource bundle.
 */
public final class ErrorCode {
    public static final String ENTITY_NOT_FOUND = "Entity is not found for Id {0}, column name {1}.";
    public static final String AGREEMENT_NOT_FOUND = "No agreement exists";
    public static final String INVALID_VERSION_PARAM_VAL =
            "start/end & max parameters to query versions of the Entity must be between 1 and {0}.";
    public static final String BAD_REQUEST = "Bad request (e.g. missing request body).";
    public static final String DELETE_INTEGRITY_ERROR =
            "Cannot delete entity with id {0} and column name {1}. It is referenced by other entities.";
}

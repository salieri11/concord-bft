/*
 * Copyright (c) 2019 VMware, Inc. All rights reserved. VMware Confidential
 */

package com.vmware.blockchain.common;

/**
 * Error Messages. At some point we will turn this into a class loading from a resource bundle.
 */
public final class ErrorCode {
    public static final String AGREEMENT_NOT_FOUND = "No agreement exists";
    public static final String BAD_REQUEST = "Bad request (e.g. missing request body).";
    public static final String BAD_LOGIN_REQUEST = "Invalid email/password";
    public static final String BAD_PASSWORD_CHANGE = "Can't use same password!";
    public static final String BAD_TOKEN = "Bad token";
    public static final String BAD_UUID_FORMAT = "Error converting {0} to UUID";
    public static final String DELETE_INTEGRITY_ERROR =
            "Cannot delete entity with id {0} and column name {1}. It is referenced by other entities.";
    public static final String ENTITY_NOT_FOUND = "Entity is not found for Id {0}, column name {1}.";
    public static final String INVALID_ROLE = "{0} is invalid Role value.";
    public static final String INVALID_VERSION_PARAM_VAL =
            "start/end & max parameters to query versions of the Entity must be between 1 and {0}.";
    public static final String ORG_NOT_FOUND = "Organization with ID {0} not found.";
    public static final String USER_ID_NOT_FOUND = "No user found with ID: {0}";
    public static final String USER_NOT_FOUND = "No user with email {0}";
}

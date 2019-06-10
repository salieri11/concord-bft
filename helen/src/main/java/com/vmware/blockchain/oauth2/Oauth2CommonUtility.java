/*
 * Copyright (c) 2019 VMware, Inc. All rights reserved. VMware Confidential
 */

package com.vmware.blockchain.oauth2;

/**
 * Utility class which has constants required and AuthToken response in lombok format.
 *
 */
public class Oauth2CommonUtility {

    public static final String STATE_KEY = "oauth-state";
    public static final String SESSION_CLEANED = "session_cleaned";
    public static final String TOKEN_EXPIRES_AT = "token-expires-at";
    public static final String TOKEN_REFRESH = "refresh-token";
    public static final String AUTH_HEADER_NAME = "csp-auth-token";
    public static final String TOKEN_ID = "token-id";
    public static final String CSP_COMMON = "/csp/gateway";
    public static final String CSP_ACCOUNT_MANAGEMENT = CSP_COMMON + "/am/api";
    public static final String CSP_ORG_API = CSP_ACCOUNT_MANAGEMENT + "/orgs";

}

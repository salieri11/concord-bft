/*
 * Copyright (c) 2019 VMware, Inc. All rights reserved. VMware Confidential
 */

package com.vmware.blockchain.common.csp;

import java.time.temporal.ChronoUnit;
import java.util.Set;

import com.google.common.collect.ImmutableSet;

/**
 * CSP related constants.  Pulling this in from VMC.  There's lots of things we don't need yet.
 */
public class CspConstants {
    public static final String VMC_SERVICE_NAME = "VMware Cloud on AWS";
    public static final String CSP_COMMON = "/csp/gateway";
    public static final String CSP_LIFECYCLE = CSP_COMMON + "/slc/api";
    public static final String CSP_ACCOUNT_MANAGEMENT = CSP_COMMON + "/am/api";
    public static final String CSP_USER_VALIDATION_API = CSP_ACCOUNT_MANAGEMENT + "/loggedin/user";
    public static final String CSP_USER_ORG_API = CSP_ACCOUNT_MANAGEMENT + "/loggedin/user/orgs?expand=1";
    public static final String CSP_ORG_API = CSP_ACCOUNT_MANAGEMENT + "/orgs";
    public static final String CSP_API_ORG_ID = CSP_ORG_API + "/{orgId}";
    public static final String CSP_USER_API = CSP_ACCOUNT_MANAGEMENT + "/users";
    public static final String CSP_SERVICE_TYPES_URL = CSP_LIFECYCLE + "/definitions";
    public static final String CSP_REGISTERED_SERVICE_INSTANCES_URL = CSP_LIFECYCLE + "/services/registered/";
    public static final String CSP_SUBSCRIPTION_ACTIVATION = CSP_LIFECYCLE + "/service-subscriptions/{id}/activate";
    public static final String CSP_SUBSCRIPTION_CANCEL = CSP_LIFECYCLE + "/service-subscriptions/{id}/cancel";
    public static final String CSP_SERVICE_LIFECYCLE_ORCHESTRATION =
            CSP_LIFECYCLE + "/lifecycle/register";
    public static final String CSP_SERVICE_DEFINITION = CSP_LIFECYCLE + "/definitions";
    public static final String CSP_BUSINESS_PLAN = CSP_COMMON + "/billing/api/business-plans";
    public static final String CSP_A_BUSINESS_PLAN = CSP_COMMON + "/billing/api/business-plans/{id}";
    public static final String CSP_BUSINESS_PLAN_FOR_SERVICE = CSP_BUSINESS_PLAN + "?serviceDefinitionLink=";
    public static final int BUSINESS_PLAN_VALIDITY = 60;
    public static final ChronoUnit BUSINESS_PLAN_UNIT = ChronoUnit.DAYS;

    public static final String CSP_VALIDATE_TOKEN = CSP_USER_API + "/tokens/{token}/isValid";
    public static final String CSP_LOGIN_API = CSP_ACCOUNT_MANAGEMENT + "/login";
    public static final String CSP_LOGIN_API_KEY = CSP_ACCOUNT_MANAGEMENT + "/auth/login/accounts/access-keys";
    public static final String CSP_LOGIN_REFRESH_TOKEN = CSP_ACCOUNT_MANAGEMENT + "/auth/api-tokens/authorize";

    // constants for oauth2
    public static final String CSP_REGISTER_OAUTH_CLIENT = CSP_ACCOUNT_MANAGEMENT + "/services/clients";
    public static final String CSP_OAUTH_TOKEN = CSP_ACCOUNT_MANAGEMENT + "/auth/authorize";
    public static final String CSP_LOGOUT = CSP_ACCOUNT_MANAGEMENT + "/auth/logout";
    public static final String GRANT_TYPE = "grant_type";
    public static final String ORG_ID = "orgId";
    public static final String CLIENT_CREDENTIALS = "client_credentials";

    public static final String CSP_DISCOVERY_PAGE = CSP_COMMON + "/discovery";

    //timeout for
    public static final long DEFAULT_TIMEOUT_IN_SECONDS = 3;
    public static final long DEFAULT_WAIT_IN_SECONDS = 1;

    public static final String AUTH_HEADER_NAME = "csp-auth-token";
    public static final String CSP_SERVICE_TOKEN = "csp-service-token";
    public static final String INVITATION_STATUS_REDEEMED = "REDEEMED";

    public static final String CSP_ROLE_PREFIX = "csp:";
    public static final String CSP_ROLE_ORG_OWNER = "csp:org_owner";
    public static final String CSP_ROLE_PLATFORM_OPERATOR = "csp:platform_operator";
    public static final String CSP_VMBC_ROLE_PREFIX = "external/";

    // Usage metering constants
    // Max size in batch of records is 100 records.
    public static final Integer CSP_USAGE_API_MAX_RECORDS_IN_BATCH_COUNT = 100;

    public static final String HEADER_CSP_REQUEST_ID = "csp-request-id";
    // Authorized party
    public static final Set<String> CSP_CALLBACK_AZPS = ImmutableSet.of("csp-billing");
    public static final String SUPPORTED_PK_ALGORITHM = "RSA";

}

/*
 * Copyright (c) 2019 VMware, Inc. All rights reserved. VMware Confidential
 */

package com.vmware.concord.agent;

/**
 * Agent service related constants.
 */
public class Constants {
    //API Endpoints
    // Below endpoints are needed because some servlets still use this information
    public static final String API_URI_PREFIX = "/api";
    public static final String STATIC_RESOURCE_LOCATION = "classpath:/static/";
    public static final String HOME_PAGE_LOCATION = "classpath:/static/index.html";

    public static final String CLIENT_VERSION = "Deployment/v1.1.0/linux/java1.8.0";

    //Other Constants.  Maybe think if these should be properties

    //Concord-Agent
    public static final String JSONRPC = "2.0";
}

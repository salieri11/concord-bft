/*
 * Copyright (c) 2019 VMware, Inc. All rights reserved. VMware Confidential
 */

package com.vmware.blockchain.oauth2;

import java.util.List;
import java.util.Map;

import lombok.Builder;
import lombok.Value;

/**
 * Holds the parameters required to construct the redirect url for authorization code.
 *
 */
@Value
@Builder
public class Oauth2AuthCodeRequestParams {
    private String baseUrl;
    private String relativePath;
    private String state;
    private String orgId;
    private String clientId;
    private String redirectUri;
    private boolean isSessionCleanRequired;
    private Map<String, List<String>> extraQueryParams;
}

/*
 * Copyright (c) 2019 VMware, Inc. All rights reserved. VMware Confidential
 */

package com.vmware.blockchain.oauth2;

import java.util.List;
import java.util.Map;

import org.springframework.http.MediaType;

import lombok.Builder;
import lombok.Value;

/**
 * Holds the parameters required to construct the redirect url for auth token.
 *
 */
@Value
@Builder
public class Oauth2AuthTokenRequestParams {
    private String baseUrl;
    private String relativePath;
    private String grantType;
    private String state;
    private String authorizationCode;
    private String clientId;
    private String clientSecret;
    private String authenticationHeader;
    private String redirectUri;
    private MediaType mediaType;
    private Map<String, List<String>> extraQueryParams;
}

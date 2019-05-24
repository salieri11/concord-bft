/*
 * Copyright (c) 2019 VMware, Inc. All rights reserved. VMware Confidential
 */

package com.vmware.blockchain.oauth2;

import org.springframework.http.MediaType;

import lombok.Builder;
import lombok.Value;

/**
 * Holds the parameters required to construct the redirect url for refresh token.
 *
 */
@Value
@Builder
public class Oauth2RefreshTokenRequestParams {
    private String baseUrl;
    private String relativePath;
    private String grantType;
    private String clientId;
    private String clientSecret;
    private String authenticationHeader;
    private String refreshToken;
    private MediaType mediaType;
    private long refreshBefore;
    private long tokenExpiresAt;
}

/*
 * Copyright (c) 2019 VMware, Inc. All rights reserved. VMware Confidential
 */

package com.vmware.blockchain.oauth2;

import com.fasterxml.jackson.annotation.JsonProperty;

import lombok.Data;

/**
 * Holds the response from the csp authorize call.
 */
@Data
public class Oauth2AuthTokenResponse {
    @JsonProperty("access_token")
    String cspAuthToken;
    @JsonProperty("refresh_token")
    String cspRefreshToken;
    @JsonProperty("id_token")
    String idToken;
    @JsonProperty("token_type")
    String tokenType;
    @JsonProperty("expires_in")
    int expiresIn;
    String scope;
    @JsonProperty("jti")
    String jti;
}

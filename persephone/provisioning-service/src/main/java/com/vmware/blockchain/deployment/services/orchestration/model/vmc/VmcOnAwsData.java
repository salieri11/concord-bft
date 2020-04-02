/*
 * Copyright (c) 2018-2019 VMware, Inc. All rights reserved. VMware Confidential
 */

package com.vmware.blockchain.deployment.services.orchestration.model.vmc;

import lombok.Data;

/**
 * Holds all the data classes.
 */
public class VmcOnAwsData {

    /**
     * Sddc resource config.
     */
    @Data
    public static class SddcResourceConfig {
        SddcResourceProvider provider;
        String vcUrl;
        String cloudUsername;
        String cloudPassword;
        String nsxMgrUrl;
        String nsxApiPublicEndpointUrl;

        /**
         * Enum as provider.
         */
        public enum SddcResourceProvider {
            AWS;
        }
    }

    /**
     * Sddc data type.
     */
    @Data
    public static class Sddc {
        SddcResourceConfig resourceConfig;
    }

    /**
     * CSP auth response.
     */
    @Data
    public static class VmcAuthenticationResponse {
        String idToken;
        String tokenType;
        int expiresIn;
        String scope;
        String accessToken;
        String refreshToken;
    }
}

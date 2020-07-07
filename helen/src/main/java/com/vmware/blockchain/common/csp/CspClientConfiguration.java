/*
 * Copyright (c) 2019 VMware, Inc. All rights reserved. VMware Confidential
 */

package com.vmware.blockchain.common.csp;

import java.util.concurrent.TimeUnit;

import org.apache.commons.lang3.StringUtils;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;

import com.vmware.blockchain.common.Constants;
import com.vmware.blockchain.common.csp.api.client.CspApiClient;
import com.vmware.blockchain.common.csp.api.client.CspApiClientBuilder;
import com.vmware.blockchain.common.restclient.interceptor.LoggingInterceptor;

/**
 * Spring configuration for csp client.
 */
@Configuration
public class CspClientConfiguration {
    private static Logger logger = LogManager.getLogger(CspClientConfiguration.class);

    @Value("${csp.url:http:/csp}")
    private String cspUrl;

    @Value("${vmbc.csp.service.user.refresh.token:#{null}}")
    private String cspServiceRefreshToken;

    @Value("${vmbc.csp.client.full.logging.enabled:false}")
    private boolean isFullLoggingEnabled;

    // client ID and secret for client credential grant.
    @Value("${vmbc.service.client.id:vmbc-client}")
    private String clientId;

    @Value("${vmbc.service.client.secret:vmbc-secret}")
    private String clientSecret;

    @Value("${vmbc.service.org.id}")
    private String orgId;

    @Value("${csp.api.logging.level:URL_STATUS}")
    private LoggingInterceptor.ApiLogLevel logLevel;

    /**
     * Create csp api client.
     *
     * @return CspApiClient instance.
     */
    @Bean(name = Constants.CSP_API_DEFAULT_CLIENT)
    public CspApiClient cspApiClient() {
        CspApiClientBuilder builder;
        builder = new CspApiClientBuilder(cspUrl);

        // TODO: We will probably get rid of this part
        if (StringUtils.isNotBlank(cspServiceRefreshToken)) {
            logger.info("Creating CSP API client at {} with refresh token defined by property {}",
                    cspUrl, "vmbc.csp.service.user.refresh.token");
            builder.withRefreshTokenAuth(cspServiceRefreshToken);
        }

        //Set the LogLevel for CSP API request/response Logging.
        builder.enableLogging(logLevel);

        if (StringUtils.isNoneBlank(clientId, clientSecret, orgId)) {
            logger.info("Creating CSP API client with client_credentials auth at {} ", cspUrl);
            builder.withClientCredentialsAuth(clientId, clientSecret, orgId);
        }

        return builder.enableRetry(10, 120, TimeUnit.SECONDS).build();
    }

}


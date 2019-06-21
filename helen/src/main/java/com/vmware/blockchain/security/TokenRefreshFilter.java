/*
 * Copyright (c) 2019 VMware, Inc. All rights reserved. VMware Confidential
 */

package com.vmware.blockchain.security;

import java.io.IOException;
import java.util.concurrent.TimeUnit;

import javax.servlet.FilterChain;
import javax.servlet.ServletException;
import javax.servlet.ServletRequest;
import javax.servlet.ServletResponse;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpSession;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.http.MediaType;
import org.springframework.stereotype.Component;
import org.springframework.web.client.HttpClientErrorException;
import org.springframework.web.filter.GenericFilterBean;

import com.vmware.blockchain.common.csp.CspConfig;
import com.vmware.blockchain.common.csp.CspConstants;
import com.vmware.blockchain.oauth2.Oauth2AuthTokenResponse;
import com.vmware.blockchain.oauth2.Oauth2CommonUtility;
import com.vmware.blockchain.oauth2.Oauth2Helper;
import com.vmware.blockchain.oauth2.Oauth2RefreshTokenRequestParams;


/**
 * Handle token refresh and session expiration VMC sessions. This filter must come before TokenAuthenticationFilter.
 */
@Component
public class TokenRefreshFilter extends GenericFilterBean {
    private String clientId;

    private String clientSecret;

    private CspConfig cspConfig;

    private static Logger logger = LogManager.getLogger(TokenRefreshFilter.class);

    @Autowired
    public TokenRefreshFilter(CspConfig cspConfig, @Value("${vmbc.client.id:ss-client}") String clientId,
            @Value("${vmbc.client.secret:ss-secret}") String clientSecret) {
        this.cspConfig = cspConfig;
        this.clientId = clientId;
        this.clientSecret = clientSecret;
    }

    @Override
    public void doFilter(ServletRequest request, ServletResponse response, FilterChain chain)
            throws IOException, ServletException {
        HttpServletRequest httpRequst = (HttpServletRequest) request;
        // Get the session, if there is one
        HttpSession session = httpRequst.getSession(false);
        if (session != null) {
            // See if we need to refresh the token
            Long tokenExpiresAt = (Long) session.getAttribute(Oauth2CommonUtility.TOKEN_EXPIRES_AT);
            if (tokenExpiresAt != null) {
                // Pick up the refresh token
                String refreshToken = (String) session.getAttribute(Oauth2CommonUtility.TOKEN_REFRESH);

                Oauth2RefreshTokenRequestParams.Oauth2RefreshTokenRequestParamsBuilder refreshTokenParamsBuilder =
                        Oauth2RefreshTokenRequestParams.builder();

                refreshTokenParamsBuilder.clientId(this.clientId).clientSecret(clientSecret)
                        .baseUrl(this.cspConfig.getCspUrl()).relativePath(CspConstants.CSP_OAUTH_TOKEN)
                        .grantType("refresh_token").mediaType(MediaType.APPLICATION_FORM_URLENCODED)
                        .refreshToken(refreshToken).tokenExpiresAt(tokenExpiresAt);
                try {
                    Oauth2AuthTokenResponse authResponse =
                            new Oauth2Helper().handleRefreshTokenFilterRequest(refreshTokenParamsBuilder.build());

                    if (authResponse != null) {
                        // get the token, then save values in session
                        String token = authResponse.getCspAuthToken();
                        session.setAttribute(Oauth2CommonUtility.AUTH_HEADER_NAME, token);
                        session.setAttribute(Oauth2CommonUtility.TOKEN_REFRESH, authResponse.getCspRefreshToken());
                        session.setAttribute(Oauth2CommonUtility.TOKEN_ID, authResponse.getIdToken());
                        long now = System.currentTimeMillis();
                        tokenExpiresAt = now + TimeUnit.SECONDS.toMillis(authResponse.getExpiresIn());
                        session.setAttribute(Oauth2CommonUtility.TOKEN_EXPIRES_AT, tokenExpiresAt);
                        logger.info("Token Refreshed");
                    }
                    // TODO: At this point, we need to notify the UI that the token has been refreshed
                } catch (HttpClientErrorException e) {
                    String errorMessage =
                            "Exception while refreshing token with csp. " + "Csp responded with status {} ";
                    logger.warn(errorMessage + " and body {}", e.getStatusCode(), e.getResponseBodyAsString());
                    // Not clear that we want to throw an error here. Refresh token may have timed out. In this
                    // case let the authentication filter deal with it.
                    chain.doFilter(request, response);
                    return;
                }
            }
        }

        chain.doFilter(request, response);
    }

}

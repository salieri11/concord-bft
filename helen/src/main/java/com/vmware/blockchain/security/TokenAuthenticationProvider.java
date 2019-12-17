/*
 * Copyright (c) 2019 VMware, Inc. All rights reserved. VMware Confidential
 */

package com.vmware.blockchain.security;

import java.security.PublicKey;
import java.util.UUID;

import javax.annotation.PostConstruct;
import javax.servlet.http.HttpServletRequest;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.apache.logging.log4j.ThreadContext;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.security.authentication.AuthenticationProvider;
import org.springframework.security.core.Authentication;
import org.springframework.security.core.AuthenticationException;
import org.springframework.security.web.authentication.preauth.PreAuthenticatedAuthenticationToken;
import org.springframework.stereotype.Component;
import org.springframework.web.context.request.RequestAttributes;
import org.springframework.web.context.request.RequestContextHolder;
import org.springframework.web.context.request.ServletRequestAttributes;

import com.vmware.blockchain.auth.AuthenticationContext;
import com.vmware.blockchain.auth.TokenValidator;
import com.vmware.blockchain.common.csp.CspConfig;
import com.vmware.blockchain.common.csp.api.client.CspApiClient;

import lombok.AllArgsConstructor;
import lombok.Data;

/**
 * Validates the token in request and returns the authorization context.
 */
@Component
public class TokenAuthenticationProvider implements AuthenticationProvider {
    private static Logger logger = LogManager.getLogger(TokenAuthenticationProvider.class);

    private CspConfig cspConfig;
    private CspApiClient cspApiClient;
    private TokenValidator tokenValidator;
    private String operatorOrg;
    private PublicKey jwtSigningPublicKey;

    /**
     * Info returned from verifyAccess.
     */
    @Data
    @AllArgsConstructor
    protected static class VerifyInfo {
        private final UUID requestOrgContext;
        private final boolean isImpersonation;
        private final boolean isCrossOrg;
    }

    /**
     * N.B. be careful about injecting other Beans - there are services (e.g. RCE) that don't include
     * a lot of the ss-base components. In particular - don't include FeatureService here.
     */
    @Autowired
    public TokenAuthenticationProvider(CspConfig cspConfig, CspApiClient cspApiClient,
            TokenValidator tokenValidator,
            @Value("${ss.operator.org:#{null}}") String operatorOrg) {
        this.cspConfig = cspConfig;
        this.cspApiClient = cspApiClient;
        this.tokenValidator = tokenValidator;
        this.operatorOrg = operatorOrg;
    }

    @PostConstruct
    public void init() throws Exception {
        jwtSigningPublicKey = cspApiClient.getJwtPublicKey();
    }

    public void setOperatorOrg(UUID orgId) {
        operatorOrg = orgId.toString();
    }

    public UUID getOperatorOrg() {
        return UUID.fromString(operatorOrg);
    }

    @Override
    public Authentication authenticate(Authentication authentication)
            throws AuthenticationException {
        PreAuthenticatedAuthenticationToken authToken =
                (PreAuthenticatedAuthenticationToken) authentication;
        logger.debug("Token received in auth provider {}", authToken.getPrincipal());
        return populateAuthContext(authToken.getPrincipal().toString());
    }

    // package private so VmbcBascicAuth can use it.
    AuthenticationContext populateAuthContext(String authToken) {


        HelenUserDetails userInfo = tokenValidator.validateAndGetAuthz(authToken);
        // Add this as soon as we can so that logging messages for Access denied etc can show username.
        ThreadContext.put("userName", userInfo.getUsername());
        ThreadContext.put("orgName", userInfo.getOrgId().toString());
        UUID userId = userInfo.getUserId();
        // set the current tenant based on tenantId in url.
        AuthenticationContext authContext =  new AuthenticationContext(userInfo, userInfo.getAuthorities());

        // If this request is coming in on an actuator, we need to use RequstContextHolder to get the
        // request info.
        RequestAttributes requestAttributes = RequestContextHolder.getRequestAttributes();
        String requestUri = "unknown";
        if (requestAttributes instanceof ServletRequestAttributes) {
            HttpServletRequest request = ((ServletRequestAttributes) requestAttributes).getRequest();
            requestUri = request.getRequestURI();
        }
        logger.info(
                "Setting authContext for {} id {} orgId {} roles {}  request {}",
                userInfo.getUsername(), userId, userInfo.getOrgId(),
                userInfo.getAuthorities(), requestUri);
        return authContext;
    }

    @Override
    public boolean supports(Class<?> authentication) {
        return authentication.equals(PreAuthenticatedAuthenticationToken.class);
    }
}

/*
 * Copyright (c) 2019 VMware, Inc. All rights reserved. VMware Confidential
 */

package com.vmware.blockchain.security;

import java.io.IOException;

import javax.servlet.FilterChain;
import javax.servlet.ServletException;
import javax.servlet.ServletRequest;
import javax.servlet.ServletResponse;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.slf4j.MDC;
import org.springframework.security.access.AccessDeniedException;
import org.springframework.security.authentication.AuthenticationManager;
import org.springframework.security.core.Authentication;
import org.springframework.security.core.AuthenticationException;
import org.springframework.security.core.context.SecurityContextHolder;
import org.springframework.security.web.authentication.preauth.PreAuthenticatedAuthenticationToken;
import org.springframework.web.filter.GenericFilterBean;

import com.vmware.blockchain.auth.AuthUtil;
import com.vmware.blockchain.auth.AuthenticationContext;
import com.vmware.blockchain.common.BadRequestException;


/**
 * Checks if the auth header token is present and passes request to authentication manager to
 * validate token and initialize authorization context for the request.
 */
public class TokenAuthenticationFilter extends GenericFilterBean {

    private static Logger logger = LoggerFactory.getLogger(TokenAuthenticationFilter.class);
    private AuthenticationManager authManager;

    /**
     * Constructor.
     */
    public TokenAuthenticationFilter(AuthenticationManager authManager) {
        this.authManager = authManager;
    }

    @Override
    public void doFilter(ServletRequest request, ServletResponse response, FilterChain filterChain)
            throws IOException, ServletException {
        HttpServletRequest httpRequest = (HttpServletRequest) request;
        HttpServletResponse httpResponse = (HttpServletResponse) response;
        final String token = AuthUtil.getToken(httpRequest);

        Authentication authentication = null;
        String requestUri = httpRequest.getRequestURI();

        try {
            // see if we can authenticate. If we can, set the security context
            // if not, pass down the chain to see if basic auth is OK
            logger.debug("Trying to authenticate user for request:{}", requestUri);
            if (token != null) {
                PreAuthenticatedAuthenticationToken authToken =
                        new PreAuthenticatedAuthenticationToken(token, null);
                authentication = authManager.authenticate(authToken);
                logger.debug("Successfully authenticated API request:{} for user:{}", requestUri,
                             ((AuthenticationContext) authentication).getUserName());
                MDC.put("userName", ((AuthenticationContext) authentication).getUserName());
                MDC.put("orgName", ((AuthenticationContext) authentication).getOrgId().toString());
                SecurityContextHolder.getContext().setAuthentication(authentication);
            }
        } catch (AuthenticationException authenticationException) {
            logger.info("Authentication failed for request:{} - {}", requestUri, authenticationException.getMessage());
            SecurityContextHolder.clearContext();
        } catch (AccessDeniedException accessDeniedException) {
            logger.info("Access denied to request:{} - {}", requestUri, accessDeniedException.getMessage());
            SecurityContextHolder.clearContext();
        } catch (BadRequestException rex) {
            SecurityContextHolder.clearContext();
        }
        filterChain.doFilter(request, response);
        MDC.remove("userName");
        MDC.remove("orgName");
    }

}

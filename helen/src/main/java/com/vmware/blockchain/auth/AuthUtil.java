/*
 * Copyright (c) 2019 VMware, Inc. All rights reserved. VMware Confidential
 */

package com.vmware.blockchain.auth;

import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpSession;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.springframework.security.core.Authentication;
import org.springframework.security.oauth2.provider.authentication.BearerTokenExtractor;

import com.vmware.blockchain.common.Constants;

/**
 * AuthUtil Some Authentication utilities, different than the authconext in AuthHelper.
 */
public class AuthUtil {
    public static Logger logger = LogManager.getLogger(AuthUtil.class);

    /**
     * Get the token from http request.
     * @param request servlet request from which to obtain token
     * @return AuthToken if there is one, or else null
     */
    public static String getToken(HttpServletRequest request) {
        String token = null;
        HttpSession session = request.getSession(false);
        if (session != null) {
            token = (String) session.getAttribute(Constants.AUTH_HEADER_NAME);
            if (token != null) {
                return token;
            }
        } else {
            logger.info("No session present");
        }

        // Add support for standard Authorization: Bearer xxx token type
        // This uses Spring's Oauth2 BearerTokenExtractor
        BearerTokenExtractor extractor = new BearerTokenExtractor();
        Authentication auth = extractor.extract(request);

        if (auth != null) {
            return (String) auth.getPrincipal();
        }

        // API calls use csp-auth-token in a header
        token = request.getHeader(Constants.AUTH_HEADER_NAME);
        if (token == null) {
            // websocket calls use token in a query paramater
            token = request.getParameter(Constants.AUTH_HEADER_NAME);
        }

        return token;
    }
}

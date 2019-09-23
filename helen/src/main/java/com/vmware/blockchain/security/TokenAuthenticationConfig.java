/*
 * Copyright (c) 2019 VMware, Inc. All rights reserved. VMware Confidential
 */

package com.vmware.blockchain.security;

import java.util.HashSet;

import javax.servlet.http.HttpServletRequest;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.springframework.security.web.csrf.CookieCsrfTokenRepository;
import org.springframework.security.web.csrf.CsrfToken;
import org.springframework.stereotype.Component;

/**
 * Various methods written to apply CSRF check to the endpoints.
 */
@Component
public class TokenAuthenticationConfig {

    Logger logger = LogManager.getLogger();

    /**
     * Apply the CSRF check.
     * @param httpRequest request
     * @param requestUri uri
     * @return flag
     */
    boolean applyCsrfCheck(HttpServletRequest httpRequest, String requestUri) {
        return true;
    }

    /**
     * Helper method to apply csrf check.
     * @param httpRequest request
     * @param tokenRepo csrfToken
     * @return flag
     */
    boolean csrfCheck(HttpServletRequest httpRequest, CookieCsrfTokenRepository tokenRepo,
                              HashSet<String> allowedMethods) {
        logger.debug("CSRF check on {}", httpRequest.getServletPath());
        // If the request does not have an httpSession, the API has been called by
        // a program, and not the ui.  Therefore, we don't need the csrf check
        if (httpRequest.getSession(false) == null) {
            return true;
        }
        // If this is one of the allowed methods (GET, HEAD, TRACE, OPTIONS), return ok
        if (allowedMethods.contains(httpRequest.getMethod())) {
            logger.debug("allowed method {}", httpRequest.getMethod());
            return true;
        }
        // otherwise, there must be an X-XSRF-TOKEN header, containing the correct csrf token
        logger.debug("Need csrf token");
        CsrfToken csrfToken = tokenRepo.loadToken(httpRequest);
        if (csrfToken != null) {
            String token = httpRequest.getHeader(csrfToken.getHeaderName());
            if (token != null && token.equals(csrfToken.getToken())) {
                return true;
            } else {
                // This is temporary to figure out why intermittently csrf checks fail from UI
                logger.info("CSRF token {}", csrfToken.getToken());
                logger.info("Header token {}", token);
            }
        } else {
            logger.info("Null csrf token from load");
        }
        logger.debug("csrf failed");
        return false;
    }
}

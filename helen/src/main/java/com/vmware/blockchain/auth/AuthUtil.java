/*
 * Copyright (c) 2019 VMware, Inc. All rights reserved. VMware Confidential
 */

package com.vmware.blockchain.auth;

import java.util.Enumeration;

import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpSession;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

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

        // Spring BearerToken extractor has been depreicated.  Replace
        // with equivalent code

        token = extractHeaderToken(request);
        if (token != null) {
            return token;
        }

        // API calls use csp-auth-token in a header
        token = request.getHeader(Constants.AUTH_HEADER_NAME);

        // BC-3654: We are not currently using websockets, so there is no
        // need for the code to look at a query param.  Use Sec-WebSocket-Protocol
        // if we need to add it.
        return token;
    }

    private static String extractHeaderToken(HttpServletRequest request) {
        Enumeration headers = request.getHeaders("Authorization");
        // There is a small but finite chance this will be a null
        if (headers == null) {
            return null;
        }

        String value;
        do {
            if (!headers.hasMoreElements()) {
                return null;
            }

            value = (String) headers.nextElement();
        } while (!value.toLowerCase().startsWith("Bearer".toLowerCase()));

        return value.substring("Bearer".length()).trim();
    }

}

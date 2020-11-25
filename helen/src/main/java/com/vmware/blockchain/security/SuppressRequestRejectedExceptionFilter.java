/*
 * Copyright (c) 2020 VMware, Inc. All rights reserved. VMware Confidential
 */

package com.vmware.blockchain.security;

import java.io.IOException;

import javax.servlet.FilterChain;
import javax.servlet.ServletException;
import javax.servlet.ServletRequest;
import javax.servlet.ServletResponse;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.springframework.core.Ordered;
import org.springframework.core.annotation.Order;
import org.springframework.http.HttpHeaders;
import org.springframework.security.web.firewall.RequestRejectedException;
import org.springframework.stereotype.Component;
import org.springframework.web.filter.GenericFilterBean;

/**
 * Filter for suppressing RequestRejectedException.
 * From https://stackoverflow.com/questions/51788764/how-to-intercept-a-requestrejectedexception-in-spring
 */
@Component
@Order(Ordered.HIGHEST_PRECEDENCE)
public class SuppressRequestRejectedExceptionFilter extends GenericFilterBean {

    Logger logger = LogManager.getLogger();

    @Override
    public void doFilter(ServletRequest req, ServletResponse res, FilterChain chain)
            throws IOException, ServletException {
        try {
            chain.doFilter(req, res);
        } catch (RequestRejectedException e) {
            HttpServletRequest request = (HttpServletRequest) req;
            HttpServletResponse response = (HttpServletResponse) res;
            logger.error("Request rejected: remote={}, user_agent={}, request_url={}", request.getRemoteHost(),
                        request.getHeader(HttpHeaders.USER_AGENT), request.getRequestURL(), e);
            response.sendError(HttpServletResponse.SC_NOT_FOUND);
        }
    }
}

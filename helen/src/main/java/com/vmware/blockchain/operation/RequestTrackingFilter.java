/*
 * Copyright (c) 2019 VMware, Inc. All rights reserved. VMware Confidential
 */

package com.vmware.blockchain.operation;

import java.io.IOException;

import javax.servlet.FilterChain;
import javax.servlet.ServletException;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.core.Ordered;
import org.springframework.core.annotation.Order;
import org.springframework.web.filter.OncePerRequestFilter;

/**
 * It will create an Operation ID (a unique request id) for each request.
 */

@Order(Ordered.HIGHEST_PRECEDENCE)
public class RequestTrackingFilter extends OncePerRequestFilter {
    private OperationContext operationContext;

    @Autowired
    public RequestTrackingFilter(OperationContext operationContext) {
        this.operationContext = operationContext;
    }

    @Override
    protected void doFilterInternal(HttpServletRequest request, HttpServletResponse response, FilterChain filterChain)
            throws ServletException, IOException {
        try {
            operationContext.initId();
            filterChain.doFilter(request, response);
        } finally {
            operationContext.removeId();
        }
    }
}
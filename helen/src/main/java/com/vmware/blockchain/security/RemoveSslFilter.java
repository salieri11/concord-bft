/*
 * Copyright (c) 2019 VMware, Inc. All rights reserved. VMware Confidential
 */

package com.vmware.blockchain.security;

import java.io.IOException;

import javax.servlet.FilterChain;
import javax.servlet.ServletException;
import javax.servlet.ServletRequest;
import javax.servlet.ServletResponse;

import org.springframework.web.filter.GenericFilterBean;

/**
 * Remove the fake ssl filter we added to fool the csrf filter into sending a secure token.
 */
public class RemoveSslFilter extends GenericFilterBean {

    @Override
    public void doFilter(ServletRequest request, ServletResponse response, FilterChain chain)
            throws IOException, ServletException {
        if (request instanceof FakeSslWrapper) {
            chain.doFilter(((FakeSslWrapper) request).getRequest(), response);
        } else {
            chain.doFilter(request, response);
        }
    }
}
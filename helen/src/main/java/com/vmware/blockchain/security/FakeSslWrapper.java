/*
 * Copyright (c) 2019 VMware, Inc. All rights reserved. VMware Confidential
 */

package com.vmware.blockchain.security;

import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletRequestWrapper;

/**
 * Request wrapper to pretend we are on a secure connection.
 */
public class FakeSslWrapper extends HttpServletRequestWrapper {
    HttpServletRequest request;

    public FakeSslWrapper(HttpServletRequest request) {
        super(request);
        this.request = request;
    }

    @Override
    public String getScheme() {
        return "https";
    }

    @Override
    public boolean isSecure() {
        return true;
    }

    public HttpServletRequest getRequest() {
        return request;
    }
}

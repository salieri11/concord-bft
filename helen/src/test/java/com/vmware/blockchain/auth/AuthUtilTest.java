/*
 * Copyright (c) 2020 VMware, Inc. All rights reserved. VMware Confidential
 */

package com.vmware.blockchain.auth;

import static com.vmware.blockchain.auth.AuthUtil.getToken;
import static org.mockito.Mockito.when;

import java.util.Collections;
import java.util.List;

import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpSession;

import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;

import com.google.common.collect.ImmutableList;
import com.vmware.blockchain.common.Constants;
import com.vmware.blockchain.common.csp.CspConstants;

/**
 * Test the AuthUtil functions.
 */
public class AuthUtilTest {

    @Mock
    private HttpServletRequest request;

    @Mock
    private HttpSession session;

    @BeforeEach
    public void init() {
        MockitoAnnotations.initMocks(this);
        when(request.getHeaders("Authorization")).thenReturn(Collections.emptyEnumeration());
    }

    @Test
    public void authInHeader() {
        when(request.getHeader(CspConstants.AUTH_HEADER_NAME)).thenReturn("atoken");
        String s = getToken(request);
        Assertions.assertEquals("atoken", s);
    }

    @Test
    public void authInSession() {
        when(request.getSession(false)).thenReturn(session);
        when(session.getAttribute(Constants.AUTH_HEADER_NAME)).thenReturn("atoken");
        String s = getToken(request);
        Assertions.assertEquals("atoken", s);
    }

    @Test
    public void noAuthInSession() {
        when(request.getSession(false)).thenReturn(session);
        String s = getToken(request);
        Assertions.assertNull(s);
    }

    @Test
    public void authInBearerToken() {
        List<String> authList = ImmutableList.of("Bearer atoken");
        when(request.getHeaders("Authorization")).thenReturn(Collections.enumeration(authList));
        String s = getToken(request);
        Assertions.assertEquals("atoken", s);
    }

    @Test
    public void authInBasicAuth() {
        // We don't handle basic auth, so this shuld be a null
        List<String> authList = ImmutableList.of("Basic atoken");
        when(request.getHeaders("Authorization")).thenReturn(Collections.enumeration(authList));
        String s = getToken(request);
        Assertions.assertNull(s);
    }

    @Test
    public void authNoHeaderAccess() {
        // There's a chance we don't have access to headers, but hey! how would anything work then?
        when(request.getHeaders("Authorization")).thenReturn(null);
        String s = getToken(request);
        Assertions.assertNull(s);
    }

    @Test
    public void authInParam() {
        when(request.getParameter(Constants.AUTH_HEADER_NAME)).thenReturn("atoken");
        String s = getToken(request);
        // auth in queryparam no longer supported
        Assertions.assertNull(s);
    }

    @Test
    public void noAuth() {
        String s = getToken(request);
        Assertions.assertNull(s);
    }

}
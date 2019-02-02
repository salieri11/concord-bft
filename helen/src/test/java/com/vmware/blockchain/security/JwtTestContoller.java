/*
 * Copyright (c) 2019 VMware, Inc. All rights reserved. VMware Confidential
 */

package com.vmware.blockchain.security;

import static com.vmware.blockchain.security.SecurityTestUtils.CONSORTIUM_ID;

import java.util.Arrays;
import java.util.List;

import org.junit.Assert;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestMethod;
import org.springframework.web.bind.annotation.RestController;

import com.vmware.blockchain.services.profiles.Roles;


/**
 * A small test controller class to handle the mockmvc calls from JwtTokenFilterTest.
 */
@RestController
public class JwtTestContoller {

    private AuthHelper authHelper;

    @Autowired
    public JwtTestContoller(AuthHelper authHelper) {
        this.authHelper = authHelper;
    }

    /**
     * Test controller to validate security context.
     */
    @RequestMapping(path = "/api/concord/eth", method = RequestMethod.GET)
    public ResponseEntity<String> doGet() {
        HelenUserDetails auth = authHelper.getDetails();
        Assert.assertNotNull(auth);
        Assert.assertEquals("user@test.com", authHelper.getEmail());
        Assert.assertEquals(CONSORTIUM_ID.toString(), authHelper.getConsortiumId());
        List<Roles> expected = Arrays.asList(Roles.ORG_USER);
        Assert.assertTrue(expected.containsAll(authHelper.getAuthorities()));
        Assert.assertEquals(expected.size(), authHelper.getAuthorities().size());
        return new ResponseEntity<String>("Tests passed", HttpStatus.OK);
    }

    @RequestMapping(path = "/api/users", method = RequestMethod.GET)
    public ResponseEntity<String> getUser() {
        return new ResponseEntity<String>("User", HttpStatus.OK);
    }

    @RequestMapping(path = "/api/auth/login", method = RequestMethod.GET)
    public ResponseEntity<String> doLogin() {
        return new ResponseEntity<String>("login", HttpStatus.OK);
    }
}

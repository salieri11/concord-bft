/*
 * Copyright (c) 2019 VMware, Inc. All rights reserved. VMware Confidential
 */

package com.vmware.blockchain.security;

import static com.vmware.blockchain.security.SecurityTestUtils.BC_ID;
import static com.vmware.blockchain.security.SecurityTestUtils.ORG_ID;

import java.util.Arrays;
import java.util.List;
import java.util.UUID;

import org.junit.Assert;
import org.junit.jupiter.api.Assertions;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.mock.mockito.MockBean;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestMethod;
import org.springframework.web.bind.annotation.RestController;

import com.vmware.blockchain.auth.AuthHelper;
import com.vmware.blockchain.base.auth.Role;
import com.vmware.blockchain.services.profiles.DefaultProfiles;
import com.vmware.blockchain.services.profiles.VmbcRoles;


/**
 * A small test controller class to handle the mockmvc calls from JwtTokenFilterTest.
 */
@RestController
public class JwtTestContoller {

    @MockBean
    DefaultProfiles defaultProfiles;

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
        Assert.assertEquals(ORG_ID, authHelper.getOrganizationId());
        List<Role> expected = Arrays.asList(VmbcRoles.ORG_USER);
        Assert.assertTrue(expected.containsAll(authHelper.getAuthorities()));
        Assert.assertEquals(expected.size(), authHelper.getAuthorities().size());
        List<UUID> chains = authHelper.getAccessChains();
        Assertions.assertEquals(1, chains.size());
        Assertions.assertEquals(BC_ID, chains.get(0));
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

    /**
     * Test operator access.
     */
    @RequestMapping(path = "/api/operator", method = RequestMethod.GET)
    public ResponseEntity<String> doOperator() {
        if (authHelper.hasAnyAuthority(VmbcRoles.systemAdmin())) {
            return new ResponseEntity<String>("operator", HttpStatus.OK);
        } else {
            return new ResponseEntity<String>("I'm sorry, Dave.  I can't do that", HttpStatus.FORBIDDEN);
        }
    }

    /**
     * Test blockchain permissions.
     */
    @RequestMapping(path = "/api/blockchain/{id}", method = RequestMethod.GET)
    public ResponseEntity<String> doBlockchain(@PathVariable UUID id) {
        if (!authHelper.hasAnyAuthority(VmbcRoles.systemAdmin()) && !authHelper.getAccessChains().contains(id)) {
            return new ResponseEntity<String>("I'm sorry, Dave.  I can't do that", HttpStatus.FORBIDDEN);
        }
        return new ResponseEntity<String>("operator", HttpStatus.OK);
    }
}

/*
 * Copyright (c) 2018 VMware, Inc. All rights reserved. VMware Confidential
 */

package com.vmware.blockchain.services.profiles;

import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import java.util.Collections;
import java.util.Map;
import java.util.Optional;

import org.json.simple.JSONObject;
import org.junit.Assert;
import org.junit.Before;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.mockito.ArgumentCaptor;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;
import org.springframework.cache.Cache;
import org.springframework.cache.CacheManager;
import org.springframework.security.crypto.password.PasswordEncoder;
import org.springframework.test.context.junit4.SpringRunner;

import com.google.common.collect.ImmutableMap;
import com.vmware.blockchain.common.UserModificationException;

/**
 * Tests for the ProfilesRegistryManager.
 */
@RunWith(SpringRunner.class)
public class ProfilesRegistryManagerTest {

    @Mock
    ConsortiumRepository consortiumRepository;

    @Mock
    OrganizationRepository organizationRepository;

    @Mock
    UserRepository userRepository;

    @Mock
    PasswordEncoder passwordEncoder;

    @Mock
    CacheManager cacheManager;

    @Mock
    Cache cache;

    @InjectMocks
    ProfilesRegistryManager prm;

    private User existingUser;
    private User newUser;
    private Organization organization;
    private Consortium consortium;

    /**
     * Initialize the mocks.
     */
    @Before
    public void init() {
        MockitoAnnotations.initMocks(this);
        MockitoAnnotations.initMocks(prm);
        // consortium and organization
        consortium = new Consortium();
        consortium.setConsortiumId(200L);
        consortium.setConsortiumName("Consortium Test");
        consortium.setConsortiumType("Test Type");
        organization = new Organization();
        organization.setOrganizationId(300L);
        organization.setOrganizationName("Test Org");
        // our test user
        existingUser = new User();
        existingUser.setUserId(101L);
        existingUser.setEmail("test@a.com");
        existingUser.setFirstName("Test");
        existingUser.setLastName("User");
        existingUser.setPassword("foobar");
        existingUser.setConsortium(consortium);
        existingUser.setOrganization(organization);
        existingUser.setRole(Roles.ORG_ADMIN);
        newUser = new User();
        newUser.setEmail("newbie@a.com");
        newUser.setFirstName("New B.");
        newUser.setLastName("User");
        newUser.setPassword("foobar");
        newUser.setConsortium(consortium);
        newUser.setOrganization(organization);
        newUser.setRole(Roles.ORG_USER);
        when(userRepository.findById(101L)).thenReturn(Optional.of(existingUser));
        when(userRepository.findUserByEmail("test@a.com")).thenReturn(Optional.of(existingUser));
        when(userRepository.save(any(User.class)))
            .thenAnswer(invocation -> invocation.getArgument(0));
        when(organizationRepository.findAll()).thenReturn(Collections.emptyList());
        when(organizationRepository.save(any(Organization.class))).thenAnswer(invocation -> invocation.getArgument(0));
        when(consortiumRepository.findAll()).thenReturn(Collections.emptyList());
        when(consortiumRepository.save(any(Consortium.class))).thenAnswer(invocation -> invocation.getArgument(0));
        when(passwordEncoder.encode(any(CharSequence.class))).thenAnswer(invocation -> invocation.getArgument(0));
        when(cacheManager.getCache(anyString())).thenReturn(cache);
    }


    @Test
    public void testGetUserWithId() throws Exception {
        JSONObject json = prm.getUserWithId("101");
        Assert.assertEquals("test@a.com", json.get("email"));
        Assert.assertEquals(101L, json.get("user_id"));
    }

    @Test
    public void testLoginGood() throws Exception {
        JSONObject json = prm.loginUser("test@a.com");
        Assert.assertEquals(Boolean.TRUE, json.get("isAuthenticated"));
        Assert.assertNotNull(json.get("last_login"));
        verify(userRepository, times(1)).save(any(User.class));
    }

    @Test(expected = UserModificationException.class)
    public void testLoginNoUser() throws Exception {
        prm.loginUser("noeone@a.com");
        Assert.fail("Should not get this far");
    }

    @Test
    public void testChangePassword() throws Exception {
        prm.changePassword("test@a.com", "arglebargle");
        Assert.assertEquals(Boolean.TRUE, prm.loginUser("test@a.com").get("isAuthenticated"));
        // save is called once by the change, and once by login
        verify(userRepository, times(2)).save(any(User.class));
    }

    @Test
    public void testCreateUser() throws Exception {
        when(organizationRepository.findById(300L)).thenReturn(Optional.of(organization));
        when(consortiumRepository.findById(200L)).thenReturn(Optional.of(consortium));
        ArgumentCaptor<User> captor = ArgumentCaptor.forClass(User.class);
        UsersApiMessage msg = new UsersApiMessage(newUser);
        String id = prm.createUser(msg);
        verify(userRepository, times(1)).save(captor.capture());
        Assert.assertEquals("0", id);
        User u = captor.getValue();
        Assert.assertEquals(newUser.getFirstName(), u.getFirstName());
        Assert.assertEquals(newUser.getLastName(), u.getLastName());
        Assert.assertEquals(newUser.getEmail(), u.getEmail());
        Assert.assertEquals(newUser.getRole(), u.getRole());
        verify(organizationRepository, times(1)).save(any(Organization.class));
        verify(consortiumRepository, times(1)).save(any(Consortium.class));
        Assert.assertTrue(organization.getUsers().contains(u));
        Assert.assertTrue(consortium.getUsers().contains(u));
    }

    @Test(expected = UserModificationException.class)
    public void testCreateExistingUser() throws Exception {
        when(organizationRepository.findById(300L)).thenReturn(Optional.of(organization));
        when(consortiumRepository.findById(200L)).thenReturn(Optional.of(consortium));
        UsersApiMessage msg = new UsersApiMessage(existingUser);
        try {
            prm.createUser(msg);
        } catch (UserModificationException e) {
            Assert.assertEquals("Duplicate email address", e.getMessage());
            verify(userRepository, times(0)).save(any());
            verify(organizationRepository, times(0)).save(any());
            verify(consortiumRepository, times(0)).save(any());
            throw e;
        }
        Assert.fail("Should not have gotten here");
    }

    @Test(expected = UserModificationException.class)
    public void testCreateUserBadOrg() throws Exception {
        when(organizationRepository.findById(300L)).thenReturn(Optional.empty());
        when(consortiumRepository.findById(200L)).thenReturn(Optional.of(consortium));
        UsersApiMessage msg = new UsersApiMessage(newUser);
        try {
            prm.createUser(msg);
        } catch (UserModificationException e) {
            Assert.assertEquals("Organization with ID 300 not found.", e.getMessage());
            verify(userRepository, times(0)).save(any());
            verify(organizationRepository, times(0)).save(any());
            verify(consortiumRepository, times(0)).save(any());
            throw e;
        }
        Assert.fail("Should not have gotten here");
    }

    @Test(expected = UserModificationException.class)
    public void testCreateUserBadConsortium() throws Exception {
        when(organizationRepository.findById(300L)).thenReturn(Optional.of(organization));
        when(consortiumRepository.findById(200L)).thenReturn(Optional.empty());
        UsersApiMessage msg = new UsersApiMessage(newUser);
        try {
            prm.createUser(msg);
        } catch (UserModificationException e) {
            Assert.assertEquals("Consortium with ID 200 not found.", e.getMessage());
            verify(userRepository, times(0)).save(any());
            verify(organizationRepository, times(0)).save(any());
            verify(consortiumRepository, times(0)).save(any());
            throw e;
        }
        Assert.fail("Should not have gotten here");
    }

    @Test(expected = UserModificationException.class)
    public void testUpdateNoUser() throws Exception {
        when(userRepository.findById(100L)).thenReturn(Optional.empty());
        UsersApiMessage msg = new UsersApiMessage();
        msg.setUserId(100L);
        try {
            prm.updateUser(msg);
        } catch (UserModificationException e) {
            Assert.assertEquals("No user found with ID: 100", e.getMessage());
            verify(userRepository, times(0)).save(any());
            verify(organizationRepository, times(0)).save(any());
            verify(consortiumRepository, times(0)).save(any());
            throw e;
        }
        Assert.fail("Should not have gotten here");
    }

    @Test(expected = UserModificationException.class)
    public void testUpdateDupEmail() throws Exception {
        Map<String, String> m = new ImmutableMap.Builder<String, String>()
                .put(UsersApiMessage.EMAIL_LABEL, "test@a.com").build();
        JSONObject json = new JSONObject(m);
        UsersApiMessage msg = new UsersApiMessage(json);
        msg.setUserId(101L);
        try {
            prm.updateUser(msg);
        } catch (UserModificationException e) {
            Assert.assertEquals("Duplicate email address", e.getMessage());
            verify(userRepository, times(0)).save(any());
            verify(organizationRepository, times(0)).save(any());
            verify(consortiumRepository, times(0)).save(any());
            throw e;
        }
        Assert.fail("Should not have gotten here");
    }

    @Test
    public void testUpdateEmailAndRole() throws Exception {
        Map<String, String> m = new ImmutableMap.Builder<String, String>()
                .put(UsersApiMessage.EMAIL_LABEL, "old-test@a.com").build();
        JSONObject json = new JSONObject(m);
        json.put("role", "ORG_ADMIN");
        UsersApiMessage msg = new UsersApiMessage(json);
        msg.setUserId(101L);
        ArgumentCaptor<User> captor = ArgumentCaptor.forClass(User.class);
        prm.updateUser(msg);
        verify(userRepository, times(1)).save(captor.capture());
        verify(organizationRepository, times(0)).save(any());
        verify(consortiumRepository, times(0)).save(any());
        // email should have changed, but not first or last name
        User u = captor.getValue();
        Assert.assertEquals("old-test@a.com", u.getEmail());
        Assert.assertEquals("Test", u.getFirstName());
        Assert.assertEquals("User", u.getLastName());
        Assert.assertEquals(Roles.ORG_ADMIN.toString(), u.getRole());
    }

    @Test(expected = UserModificationException.class)
    public void testUpdateBadRold() throws Exception {
        Map<String, String> m = new ImmutableMap.Builder<String, String>()
                .put(UsersApiMessage.ROLE_LABEL, "invalid_role").build();
        JSONObject json = new JSONObject(m);
        UsersApiMessage msg = new UsersApiMessage(json);
        msg.setUserId(101L);
        try {
            prm.updateUser(msg);
        } catch (UserModificationException e) {
            Assert.assertEquals("Invalid role value: invalid_role", e.getMessage());
            verify(userRepository, times(0)).save(any());
            verify(organizationRepository, times(0)).save(any());
            verify(consortiumRepository, times(0)).save(any());
            throw e;
        }
        Assert.fail("Should not have gotten here");
    }

    @Test
    public void testLoginUser() throws Exception {
        prm.loginUser(existingUser);
        Assert.assertNotEquals(new Long(0), existingUser.getLastLogin());
    }

}

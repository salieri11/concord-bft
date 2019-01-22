/*
 * Copyright (c) 2018-2019 VMware, Inc. All rights reserved. VMware Confidential
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
import java.util.UUID;

import org.json.simple.JSONObject;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.ArgumentCaptor;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;
import org.springframework.cache.Cache;
import org.springframework.cache.CacheManager;
import org.springframework.security.crypto.password.PasswordEncoder;
import org.springframework.test.context.junit.jupiter.SpringExtension;

import com.google.common.collect.ImmutableMap;
import com.vmware.blockchain.common.EntityModificationException;

/**
 * Tests for the ProfilesRegistryManager.
 */
@ExtendWith(SpringExtension.class)
class ProfilesRegistryManagerTest {
    // Just some random UUIDs
    private static final UUID USER_ID = UUID.fromString("f1c1aa4f-4958-4e93-8a51-930d595fb65b");
    private static final UUID NEW_USER_ID = UUID.fromString("f7e5d195-5281-4c7f-b719-3b6b40a736f2");
    private static final UUID ORG_ID = UUID.fromString("82634974-88cf-4944-a99d-6b92664bb765");
    private static final UUID CONSORTIUM_ID = UUID.fromString("5c7cd0e9-57ad-44af-902f-74af2f3dd8fe");


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
    @BeforeEach
    void init() {
        MockitoAnnotations.initMocks(this);
        MockitoAnnotations.initMocks(prm);
        // consortium and organization
        consortium = new Consortium();
        consortium.setConsortiumId(CONSORTIUM_ID);
        consortium.setConsortiumName("Consortium Test");
        consortium.setConsortiumType("Test Type");
        organization = new Organization();
        organization.setOrganizationId(ORG_ID);
        organization.setOrganizationName("Test Org");
        // our test user
        existingUser = new User();
        existingUser.setUserId(USER_ID);
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
        when(userRepository.findById(USER_ID)).thenReturn(Optional.of(existingUser));
        when(userRepository.findUserByEmail("test@a.com")).thenReturn(Optional.of(existingUser));
        when(userRepository.save(any(User.class)))
            .thenAnswer(invocation -> {
                User u = invocation.getArgument(0);
                if (u.getUserId() == null) {
                    u.setUserId(NEW_USER_ID);
                }
                return u;
            });
        when(organizationRepository.findAll()).thenReturn(Collections.emptyList());
        when(organizationRepository.save(any(Organization.class))).thenAnswer(invocation -> invocation.getArgument(0));
        when(consortiumRepository.findAll()).thenReturn(Collections.emptyList());
        when(consortiumRepository.save(any(Consortium.class))).thenAnswer(invocation -> invocation.getArgument(0));
        when(passwordEncoder.encode(any(CharSequence.class))).thenAnswer(invocation -> invocation.getArgument(0));
        when(cacheManager.getCache(anyString())).thenReturn(cache);
    }


    @Test
    void testGetUserWithId() {
        JSONObject json = prm.getUserWithId(USER_ID.toString());
        Assertions.assertEquals("test@a.com", json.get("email"));
        Assertions.assertEquals(USER_ID, json.get("user_id"));
    }

    @Test
    void testLoginGood() {
        JSONObject json = prm.loginUser("test@a.com");
        Assertions.assertEquals(Boolean.TRUE, json.get("isAuthenticated"));
        Assertions.assertNotNull(json.get("last_login"));
        verify(userRepository, times(1)).save(any(User.class));
    }

    @Test
    void testLoginNoUser() {
        Assertions.assertThrows(
            EntityModificationException.class,
            () -> prm.loginUser("noeone@a.com"));
    }

    @Test
    void testChangePassword() {
        prm.changePassword("test@a.com", "arglebargle");
        Assertions.assertEquals(Boolean.TRUE, prm.loginUser("test@a.com").get("isAuthenticated"));
        // save is called once by the change, and once by login
        verify(userRepository, times(2)).save(any(User.class));
    }

    @Test
    void testCreateUser() {
        when(organizationRepository.findById(ORG_ID)).thenReturn(Optional.of(organization));
        when(consortiumRepository.findById(CONSORTIUM_ID)).thenReturn(Optional.of(consortium));
        ArgumentCaptor<User> captor = ArgumentCaptor.forClass(User.class);
        UsersApiMessage msg = new UsersApiMessage(newUser);
        String id = prm.createUser(msg);
        verify(userRepository, times(1)).save(captor.capture());
        Assertions.assertEquals(NEW_USER_ID.toString(), id);
        User u = captor.getValue();
        Assertions.assertEquals(newUser.getFirstName(), u.getFirstName());
        Assertions.assertEquals(newUser.getLastName(), u.getLastName());
        Assertions.assertEquals(newUser.getEmail(), u.getEmail());
        Assertions.assertEquals(newUser.getRole(), u.getRole());
        verify(organizationRepository, times(1)).save(any(Organization.class));
        verify(consortiumRepository, times(1)).save(any(Consortium.class));
        Assertions.assertTrue(organization.getUsers().contains(u));
        Assertions.assertTrue(consortium.getUsers().contains(u));
    }

    @Test
    void testCreateExistingUser() {
        when(organizationRepository.findById(ORG_ID)).thenReturn(Optional.of(organization));
        when(consortiumRepository.findById(CONSORTIUM_ID)).thenReturn(Optional.of(consortium));
        UsersApiMessage msg = new UsersApiMessage(existingUser);
        Assertions.assertThrows(EntityModificationException.class, () -> {
            try {
                prm.createUser(msg);
            } catch (EntityModificationException e) {
                Assertions.assertEquals("Duplicate email address", e.getMessage());
                verify(userRepository, times(0)).save(any());
                verify(organizationRepository, times(0)).save(any());
                verify(consortiumRepository, times(0)).save(any());
                throw e;
            }
        });
    }

    @Test
    void testCreateUserBadOrg() {
        when(organizationRepository.findById(ORG_ID)).thenReturn(Optional.empty());
        when(consortiumRepository.findById(CONSORTIUM_ID)).thenReturn(Optional.of(consortium));
        UsersApiMessage msg = new UsersApiMessage(newUser);
        Assertions.assertThrows(EntityModificationException.class, () -> {
            try {
                prm.createUser(msg);
            } catch (EntityModificationException e) {
                Assertions.assertEquals(
                    "Organization with ID 82634974-88cf-4944-a99d-6b92664bb765 not found.",
                    e.getMessage());
                verify(userRepository, times(0)).save(any());
                verify(organizationRepository, times(0)).save(any());
                verify(consortiumRepository, times(0)).save(any());
                throw e;
            }
        });
    }

    @Test
    void testCreateUserBadConsortium() {
        when(organizationRepository.findById(ORG_ID)).thenReturn(Optional.of(organization));
        when(consortiumRepository.findById(CONSORTIUM_ID)).thenReturn(Optional.empty());
        UsersApiMessage msg = new UsersApiMessage(newUser);
        Assertions.assertThrows(EntityModificationException.class, () -> {
            try {
                prm.createUser(msg);
            } catch (EntityModificationException e) {
                Assertions.assertEquals(
                    "Consortium with ID 5c7cd0e9-57ad-44af-902f-74af2f3dd8fe not found.",
                    e.getMessage());
                verify(userRepository, times(0)).save(any());
                verify(organizationRepository, times(0)).save(any());
                verify(consortiumRepository, times(0)).save(any());
                throw e;
            }
        });
    }

    @Test
    void testUpdateNoUser() {
        when(userRepository.findById(USER_ID)).thenReturn(Optional.empty());
        UsersApiMessage msg = new UsersApiMessage();
        msg.setUserId(USER_ID);
        Assertions.assertThrows(EntityModificationException.class, () -> {
            try {
                prm.updateUser(msg);
            } catch (EntityModificationException e) {
                Assertions.assertEquals("No user found with ID: " + USER_ID, e.getMessage());
                verify(userRepository, times(0)).save(any());
                verify(organizationRepository, times(0)).save(any());
                verify(consortiumRepository, times(0)).save(any());
                throw e;
            }
        });
    }

    @Test
    void testUpdateDupEmail() {
        Map<String, String> m = new ImmutableMap.Builder<String, String>()
                .put(UsersApiMessage.EMAIL_LABEL, "test@a.com").build();
        JSONObject json = new JSONObject(m);
        UsersApiMessage msg = new UsersApiMessage(json);
        msg.setUserId(USER_ID);
        Assertions.assertThrows(EntityModificationException.class, () -> {
            try {
                prm.updateUser(msg);
            } catch (EntityModificationException e) {
                Assertions.assertEquals("Duplicate email address", e.getMessage());
                verify(userRepository, times(0)).save(any());
                verify(organizationRepository, times(0)).save(any());
                verify(consortiumRepository, times(0)).save(any());
                throw e;
            }
        });
    }

    @Test
    void testUpdateEmailAndRole() {
        Map<String, String> m = new ImmutableMap.Builder<String, String>()
                .put(UsersApiMessage.EMAIL_LABEL, "old-test@a.com").build();
        JSONObject json = new JSONObject(m);
        @SuppressWarnings("unchecked") Map<String, String> jsonAsMap = (Map<String, String>) json;
        jsonAsMap.put("role", "ORG_ADMIN");
        UsersApiMessage msg = new UsersApiMessage(json);
        msg.setUserId(USER_ID);
        ArgumentCaptor<User> captor = ArgumentCaptor.forClass(User.class);
        prm.updateUser(msg);
        verify(userRepository, times(1)).save(captor.capture());
        verify(organizationRepository, times(0)).save(any());
        verify(consortiumRepository, times(0)).save(any());
        // email should have changed, but not first or last name
        User u = captor.getValue();
        Assertions.assertEquals("old-test@a.com", u.getEmail());
        Assertions.assertEquals("Test", u.getFirstName());
        Assertions.assertEquals("User", u.getLastName());
        Assertions.assertEquals(Roles.ORG_ADMIN.toString(), u.getRole());
    }

    @Test
    void testUpdateBadRold() {
        Map<String, String> m = new ImmutableMap.Builder<String, String>()
                .put(UsersApiMessage.ROLE_LABEL, "invalid_role").build();
        JSONObject json = new JSONObject(m);
        UsersApiMessage msg = new UsersApiMessage(json);
        msg.setUserId(USER_ID);
        Assertions.assertThrows(EntityModificationException.class, () -> {
            try {
                prm.updateUser(msg);
            } catch (EntityModificationException e) {
                Assertions.assertEquals("Invalid role value: invalid_role", e.getMessage());
                verify(userRepository, times(0)).save(any());
                verify(organizationRepository, times(0)).save(any());
                verify(consortiumRepository, times(0)).save(any());
                throw e;
            }
        });
    }

    @Test
    void testLoginUser() {
        prm.loginUser(existingUser);
        Assertions.assertNotEquals(0, existingUser.getLastLogin());
    }

}

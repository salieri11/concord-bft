/*
 * Copyright (c) 2019 VMware, Inc. All rights reserved. VMware Confidential
 */

package com.vmware.blockchain.services.profiles;

import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import java.util.Collections;
import java.util.UUID;

import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.AdditionalMatchers;
import org.mockito.ArgumentCaptor;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.springframework.cache.Cache;
import org.springframework.cache.CacheManager;
import org.springframework.security.crypto.password.PasswordEncoder;
import org.springframework.test.context.junit.jupiter.SpringExtension;

import com.vmware.blockchain.common.EntityModificationException;
import com.vmware.blockchain.common.NotFoundException;

/**
 * Tests for the ProfilesRegistryManager.
 */
@ExtendWith(SpringExtension.class)
class ProfilesServiceTest {
    // Just some random UUIDs
    private static final UUID USER_ID = UUID.fromString("f1c1aa4f-4958-4e93-8a51-930d595fb65b");
    private static final UUID NEW_USER_ID = UUID.fromString("f7e5d195-5281-4c7f-b719-3b6b40a736f2");
    private static final UUID ORG_ID = UUID.fromString("82634974-88cf-4944-a99d-6b92664bb765");
    private static final UUID CONSORTIUM_ID = UUID.fromString("5c7cd0e9-57ad-44af-902f-74af2f3dd8fe");


    @Mock
    ConsortiumService consortiumService;

    @Mock
    OrganizationService organizationService;

    @Mock
    UserService userService;

    @Mock
    PasswordEncoder passwordEncoder;

    @Mock
    CacheManager cacheManager;

    @Mock
    Cache cache;

    @InjectMocks
    ProfilesService prm;

    private User existingUser;
    private User newUser;
    private Organization organization;
    private Consortium consortium;

    /**
     * Initialize the mocks.
     */
    @BeforeEach
    void init() {

        // consortium and organization
        consortium = new Consortium();
        consortium.setId(CONSORTIUM_ID);
        consortium.setConsortiumName("Consortium Test");
        consortium.setConsortiumType("Test Type");
        organization = new Organization();
        organization.setId(ORG_ID);
        organization.setOrganizationName("Test Org");
        // our test user
        existingUser = new User();
        existingUser.setId(USER_ID);
        existingUser.setEmail("test@a.com");
        existingUser.setFirstName("Test");
        existingUser.setLastName("User");
        existingUser.setPassword("foobar");
        existingUser.setOrganization(ORG_ID);
        existingUser.setServiceRoles(Collections.singletonList(VmbcRoles.ORG_ADMIN));
        newUser = new User();
        newUser.setEmail("newbie@a.com");
        newUser.setFirstName("New B.");
        newUser.setLastName("User");
        newUser.setPassword("foobar");
        newUser.setOrganization(ORG_ID);
        newUser.setServiceRoles(Collections.singletonList(VmbcRoles.ORG_USER));
        when(userService.get(USER_ID)).thenReturn(existingUser);
        when(userService.getByEmail("test@a.com")).thenReturn(existingUser);
        when(userService.get(AdditionalMatchers.not(eq(USER_ID))))
            .thenThrow(new NotFoundException("Not found"));
        when(userService.getByEmail(AdditionalMatchers.not(eq("test@a.com"))))
            .thenThrow(new NotFoundException("Not found"));
        when(userService.put(any(User.class)))
            .thenAnswer(invocation -> {
                User u = invocation.getArgument(0);
                if (u.getId() == null) {
                    u.setId(NEW_USER_ID);
                }
                return u;
            });
        when(organizationService.list()).thenReturn(Collections.emptyList());
        when(organizationService.put(any(Organization.class))).thenAnswer(invocation -> invocation.getArgument(0));
        when(consortiumService.list()).thenReturn(Collections.emptyList());
        when(consortiumService.put(any(Consortium.class))).thenAnswer(invocation -> invocation.getArgument(0));
        when(passwordEncoder.encode(any(CharSequence.class))).thenAnswer(invocation -> invocation.getArgument(0));
        when(cacheManager.getCache(anyString())).thenReturn(cache);
    }


    @Test
    void testGetUserWithId() {
        User user = prm.getUserWithId(USER_ID.toString());
        Assertions.assertEquals("test@a.com", user.getEmail());
        Assertions.assertEquals(USER_ID, user.getId());
    }

    @Test
    void testLoginGood() {
        prm.loginUser(existingUser);
        Assertions.assertNotNull(existingUser.getLastLogin());
        verify(userService, times(1)).put(any(User.class));
    }

    @Test
    void testChangePassword() {
        prm.changePassword(existingUser, "arglebargle");
        prm.loginUser(existingUser);
        // save is called once by the change, and once by login
        verify(userService, times(2)).put(any(User.class));
    }

    @Test
    void testUpdateNoUser() {
        when(userService.get(USER_ID)).thenThrow(new NotFoundException("not found"));
        UserPatchRequest msg = new UserPatchRequest();
        msg.setUserId(USER_ID);
        Assertions.assertThrows(EntityModificationException.class, () -> {
            try {
                prm.updateUser(msg);
            } catch (EntityModificationException e) {
                Assertions.assertEquals("No user found with ID " + USER_ID, e.getMessage());
                Assertions.assertEquals(400, e.getHttpStatus().value());
                verify(userService, times(0)).put(any());
                throw e;
            }
        });
    }

    @Test
    void testUpdateDupEmail() {
        UserPatchRequest msg = new UserPatchRequest();
        msg.setUserId(USER_ID);
        msg.setEmail("test@a.com");
        Assertions.assertThrows(EntityModificationException.class, () -> {
            try {
                prm.updateUser(msg);
            } catch (EntityModificationException e) {
                Assertions.assertEquals("Duplicate email address", e.getMessage());
                Assertions.assertEquals(400, e.getHttpStatus().value());
                verify(userService, times(0)).put(any());
                throw e;
            }
        });
    }

    @Test
    void testUpdateEmailAndRole() {
        UserPatchRequest msg = new UserPatchRequest();
        msg.setUserId(USER_ID);
        msg.setEmail("old-test@a.com");
        msg.setRole("vmbc-org:admin");
        ArgumentCaptor<User> captor = ArgumentCaptor.forClass(User.class);
        prm.updateUser(msg);
        verify(userService, times(1)).put(captor.capture());
        // email should have changed, but not first or last name
        User u = captor.getValue();
        Assertions.assertEquals("old-test@a.com", u.getEmail());
        Assertions.assertEquals("Test", u.getFirstName());
        Assertions.assertEquals("User", u.getLastName());
        Assertions.assertEquals(VmbcRoles.ORG_ADMIN.getName(), u.getServiceRoles().get(0).getName());
    }

    @Test
    void testUpdateBadRole() {
        UserPatchRequest msg = new UserPatchRequest();
        msg.setUserId(USER_ID);
        msg.setRole("invalid_role");
        Assertions.assertThrows(EntityModificationException.class, () -> {
            try {
                prm.updateUser(msg);
            } catch (EntityModificationException e) {
                Assertions.assertEquals("Invalid role value: invalid_role", e.getMessage());
                Assertions.assertEquals(400, e.getHttpStatus().value());
                verify(userService, times(0)).put(any());
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

/*
 * Copyright (c) 2018 VMware, Inc. All rights reserved. VMware Confidential
 */

package services.profiles;

import static org.mockito.ArgumentMatchers.any;
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
import org.springframework.test.context.junit4.SpringRunner;

import com.google.common.collect.ImmutableMap;

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
        consortium.setConsortiumID(200L);
        consortium.setConsortiumName("Consortium Test");
        consortium.setConsortiumType("Test Type");
        organization = new Organization();
        organization.setOrganizationID(300L);
        organization.setOrganizationName("Test Org");
        // our test user
        existingUser = new User();
        existingUser.setUserID(101L);
        existingUser.setEmail("test@a.com");
        existingUser.setFirstName("Test");
        existingUser.setLastName("User");
        existingUser.setPassword("foobar");
        existingUser.setConsortium(consortium);
        existingUser.setOrganization(organization);
        existingUser.setRole("org_user");
        newUser = new User();
        newUser.setEmail("newbie@a.com");
        newUser.setFirstName("New B.");
        newUser.setLastName("User");
        newUser.setPassword("foobar");
        newUser.setConsortium(consortium);
        newUser.setOrganization(organization);
        newUser.setRole("org_user");
        when(userRepository.findById(101L)).thenReturn(Optional.of(existingUser));
        when(userRepository.findUserByEmail("test@a.com")).thenReturn(Optional.of(existingUser));
        when(userRepository.save(any(User.class)))
            .thenAnswer(invocation -> invocation.getArgument(0));
        when(organizationRepository.findAll()).thenReturn(Collections.emptyList());
        when(organizationRepository.save(any(Organization.class)))
            .thenAnswer(invocation -> invocation.getArgument(0));
        when(consortiumRepository.findAll()).thenReturn(Collections.emptyList());
        when(consortiumRepository.save(any(Consortium.class)))
            .thenAnswer(invocation -> invocation.getArgument(0));
    }


    @Test
    public void testGetUserWithId() throws Exception {
        JSONObject json = prm.getUserWithID("101");
        Assert.assertEquals("test@a.com", json.get("email"));
        Assert.assertEquals(101L, json.get("user_id"));
    }

    @Test
    public void testLoginGood() throws Exception {
        JSONObject json = prm.loginUser("test@a.com", "foobar");
        Assert.assertEquals(Boolean.TRUE, json.get("isAuthenticated"));
        Assert.assertNotNull(json.get("last_login"));
        verify(userRepository, times(1)).save(any(User.class));
    }

    @Test
    public void testLogingBad() throws Exception {
        JSONObject json = prm.loginUser("test@a.com", "arglebargle");
        Assert.assertEquals(Boolean.FALSE, json.get("isAuthenticated"));
    }

    @Test(expected = UserModificationException.class)
    public void testLoginNoUser() throws Exception {
        prm.loginUser("noeone@a.com", "arglebargle");
        Assert.fail("Should not get this far");
    }

    @Test
    public void testChangePassword() throws Exception {
        prm.changePassword("test@a.com", "arglebargle");
        Assert.assertEquals(Boolean.TRUE, prm.loginUser("test@a.com", "arglebargle").get("isAuthenticated"));
        // save is called once by the change, and once by login
        verify(userRepository, times(2)).save(any(User.class));
    }

    @Test
    public void testCreateOrg() throws Exception {
        ArgumentCaptor<Organization> captor = ArgumentCaptor.forClass(Organization.class);
        Long l = prm.createOrgIfNotExist();
        verify(organizationRepository, times(1)).save(captor.capture());
        Assert.assertEquals("TEST_ORG", captor.getValue().getOrganizationName());
        // since we're just creating a new one, ID will be zero in unit tests
        Assert.assertEquals(Long.valueOf(0), l);
    }

    @Test
    public void testCreateOrgExits() throws Exception {
        when(organizationRepository.findAll()).thenReturn(Collections.singletonList(organization));
        Long l = prm.createOrgIfNotExist();
        Assert.assertEquals(Long.valueOf(300), l);
        verify(organizationRepository, times(0)).save(any(Organization.class));
    }

    @Test
    public void testCreateConsortium() throws Exception {
        ArgumentCaptor<Consortium> captor = ArgumentCaptor.forClass(Consortium.class);
        prm.createConsortiumIfNotExist();
        verify(consortiumRepository, times(1)).save(captor.capture());
        Assert.assertEquals("TEST_CON", captor.getValue().getConsortiumName());
        Assert.assertEquals("ATHENA", captor.getValue().getConsortiumType());
    }

    @Test
    public void testCreateConsortiumExits() throws Exception {
        when(consortiumRepository.findAll()).thenReturn(Collections.singletonList(consortium));
        Long l = prm.createConsortiumIfNotExist();
        verify(consortiumRepository, times(0)).save(any(Consortium.class));
        Assert.assertEquals(Long.valueOf(200), l);
    }

    @Test
    public void testCreateUser() throws Exception {
        when(organizationRepository.findById(300L)).thenReturn(Optional.of(organization));
        when(consortiumRepository.findById(200L)).thenReturn(Optional.of(consortium));
        ArgumentCaptor<User> captor = ArgumentCaptor.forClass(User.class);
        UsersAPIMessage msg = new UsersAPIMessage(newUser);
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
        UsersAPIMessage msg = new UsersAPIMessage(existingUser);
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
        UsersAPIMessage msg = new UsersAPIMessage(newUser);
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
        UsersAPIMessage msg = new UsersAPIMessage(newUser);
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
    public void testCreateUserBadRole() throws Exception {
        when(organizationRepository.findById(300L)).thenReturn(Optional.of(organization));
        when(consortiumRepository.findById(200L)).thenReturn(Optional.of(consortium));
        newUser.setRole("dice_role");
        UsersAPIMessage msg = new UsersAPIMessage(newUser);
        try {
            prm.createUser(msg);
        } catch (UserModificationException e) {
            Assert.assertEquals("dice_role is invalid Role value.", e.getMessage());
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
        UsersAPIMessage msg = new UsersAPIMessage();
        msg.setUserID(100L);
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
                .put(UsersAPIMessage.EMAIL_LABEL, "test@a.com").build();
        JSONObject json = new JSONObject(m);
        UsersAPIMessage msg = new UsersAPIMessage(json);
        msg.setUserID(101L);
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
                .put(UsersAPIMessage.EMAIL_LABEL, "old-test@a.com")
                .put(UsersAPIMessage.ROLE_LABEL, "org_admin").build();
        JSONObject json = new JSONObject(m);
        UsersAPIMessage msg = new UsersAPIMessage(json);
        msg.setUserID(101L);
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
        Assert.assertEquals("org_admin", u.getRole());
    }

    @Test(expected = UserModificationException.class)
    public void testUpdateBadRold() throws Exception {
        Map<String, String> m = new ImmutableMap.Builder<String, String>()
                .put(UsersAPIMessage.ROLE_LABEL, "invalid_role").build();
        JSONObject json = new JSONObject(m);
        UsersAPIMessage msg = new UsersAPIMessage(json);
        msg.setUserID(101L);
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


}

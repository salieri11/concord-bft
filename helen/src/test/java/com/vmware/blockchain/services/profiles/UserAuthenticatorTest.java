/*
 * Copyright (c) 2018 VMware, Inc. All rights reserved. VMware Confidential
 */

package com.vmware.blockchain.services.profiles;

import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.Mockito.doAnswer;
import static org.mockito.Mockito.when;
import static org.springframework.security.test.web.servlet.request.SecurityMockMvcRequestPostProcessors.csrf;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.post;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.content;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.status;

import java.util.Optional;

import org.junit.Assert;
import org.junit.Before;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.autoconfigure.web.servlet.WebMvcTest;
import org.springframework.boot.test.mock.mockito.MockBean;
import org.springframework.context.annotation.ComponentScan;
import org.springframework.http.MediaType;
import org.springframework.security.authentication.TestingAuthenticationToken;
import org.springframework.security.crypto.password.PasswordEncoder;
import org.springframework.test.context.ContextConfiguration;
import org.springframework.test.context.junit4.SpringRunner;
import org.springframework.test.web.servlet.MockMvc;
import org.springframework.test.web.servlet.result.MockMvcResultHandlers;

import com.vmware.blockchain.MvcConfig;
import com.vmware.blockchain.common.ConcordProperties;
import com.vmware.blockchain.connections.ConcordConnectionPool;
import com.vmware.blockchain.security.JwtTokenProvider;

/**
 * User Authenticator tests.
 * Initialize with MvcConfig, and only scan this compoenent.
 */
@RunWith(SpringRunner.class)
@WebMvcTest(secure = false, controllers = UserAuthenticator.class)
@ContextConfiguration(classes = MvcConfig.class)
@ComponentScan(basePackageClasses = { UserAuthenticatorTest.class })
public class UserAuthenticatorTest {

    @Autowired
    private MockMvc mvc;

    @MockBean
    private UserRepository userRepository;

    @MockBean
    private ProfilesRegistryManager prm;

    @MockBean
    private PasswordEncoder passwordEncoder;

    @MockBean
    private JwtTokenProvider jwtTokenProvider;

    @MockBean
    private ConcordProperties concordProperties;

    @MockBean
    private ConcordConnectionPool connectionPool;

    @MockBean
    private AgreementRepository agreementRepository;

    private User testUser;

    /**
     * initialize test user and mocks.
     */
    @Before
    public void init() {
        Consortium consortium = new Consortium();
        consortium.setConsortiumId(200L);
        consortium.setConsortiumName("Consortium Test");
        consortium.setConsortiumType("Test Type");
        Organization organization = new Organization();
        organization.setOrganizationId(300L);
        organization.setOrganizationName("Test Org");
        // our test user
        testUser = new User();
        testUser.setUserId(20L);
        testUser.setEmail("user@test.com");
        testUser.setFirstName("Test");
        testUser.setLastName("User");
        testUser.setPassword("1234");
        testUser.setConsortium(consortium);
        testUser.setOrganization(organization);
        testUser.setRole(Roles.SYSTEM_ADMIN);

        // The order of these matters.  When the user is "user@test.com" return the test user,
        // otherwise, return and empty optional.
        when(userRepository.findUserByEmail(anyString())).thenReturn(Optional.empty());
        when(userRepository.findUserByEmail("user@test.com")).thenReturn(Optional.of(testUser));

        when(passwordEncoder.matches(anyString(), anyString())).thenReturn(false);
        when(passwordEncoder.matches("1234", "1234")).thenReturn(true);
        when(passwordEncoder.encode(anyString())).then(a -> a.getArguments().toString());

        jwtTokenProvider.validityInMilliseconds = 1800000;
        when(jwtTokenProvider.createToken(any(User.class))).thenReturn("token");
        when(jwtTokenProvider.createRefreshToken(any(User.class))).thenReturn("refresh_token");
        when(jwtTokenProvider.validateToken(anyString())).thenReturn(false);
        when(jwtTokenProvider.validateToken("token")).thenReturn(true);
        when(jwtTokenProvider.getEmail("token")).thenReturn("user@test.com");
        when(jwtTokenProvider.getAuthentication("token"))
            .thenReturn(new TestingAuthenticationToken("user@test.com", "1234"));

        doAnswer(invocation -> {
            User u = invocation.getArgument(0);
            u.setLastLogin(1000L);
            return null;
        }).when(prm).loginUser(any(User.class));
    }


    @Test
    public void loginTest() throws Exception {
        String loginRequest = "{\"email\": \"user@test.com\", \"password\": \"1234\"}";
        String loginResponse =
                "{\"user_id\":20, \"last_login\":0, \"token\":\"token\",\"refresh_token\":"
                + "\"refresh_token\",\"token_expires\":1800000}";
        mvc.perform(post("/api/auth/login")
                .contentType(MediaType.APPLICATION_JSON).content(loginRequest))
                .andDo(MockMvcResultHandlers.print())
                .andExpect(status().isOk()).andExpect(content().json(loginResponse));
        Assert.assertNotEquals(new Long(0), testUser.getLastLogin());
    }

    @Test
    public void missingParamTest() throws Exception {
        String loginRequest = "{\"email\": \"user@test.com\"}";
        String loginResponse = "{\"error\":\"Invalid email/password\"}";
        mvc.perform(post("/api/auth/login").with(csrf()).with(csrf())
                .contentType(MediaType.APPLICATION_JSON).content(loginRequest))
                .andExpect(status().is(401)).andExpect(content().json(loginResponse));
    }

    @Test
    public void badPasswordTest() throws Exception {
        String loginRequest = "{\"email\": \"user@test.com\", \"password\": \"3456\"}";
        String loginResponse = "{\"error\":\"Invalid email/password\"}";
        mvc.perform(post("/api/auth/login").with(csrf())
                .contentType(MediaType.APPLICATION_JSON).content(loginRequest))
                .andExpect(status().is(401)).andExpect(content().json(loginResponse));
    }

    @Test
    public void noUserTest() throws Exception {
        String loginRequest = "{\"email\": \"baduser@test.com\", \"password\": \"1234\"}";
        String loginResponse = "{\"error\":\"Invalid email/password\"}";
        mvc.perform(post("/api/auth/login").with(csrf())
                .contentType(MediaType.APPLICATION_JSON).content(loginRequest))
            .andExpect(status().is(401)).andExpect(content().json(loginResponse));
    }

    @Test
    public void tokenTest() throws Exception {
        String tokenRequest = "{\"refresh_token\": \"token\"}";
        String tokenResponse = "{\"refresh_token\":\"refresh_token\",\"token_expires\":1800000,\"token\":\"token\"}";
        mvc.perform(post("/api/auth/token").with(csrf())
                .characterEncoding("utf-8")
                .content(tokenRequest).contentType(MediaType.APPLICATION_JSON))
            .andExpect(status().isOk()).andExpect(content().json(tokenResponse));
    }

}

/*
 * Copyright (c) 2018-2019 VMware, Inc. All rights reserved. VMware Confidential
 */

package com.vmware.blockchain.services.profiles;

import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.doAnswer;
import static org.mockito.Mockito.when;
import static org.springframework.security.test.web.servlet.request.SecurityMockMvcRequestPostProcessors.csrf;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.post;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.content;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.status;

import java.time.Instant;
import java.util.Arrays;
import java.util.Collections;
import java.util.UUID;

import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.AdditionalMatchers;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.autoconfigure.web.servlet.WebMvcTest;
import org.springframework.context.annotation.ComponentScan;
import org.springframework.http.MediaType;
import org.springframework.security.core.authority.SimpleGrantedAuthority;
import org.springframework.security.crypto.password.PasswordEncoder;
import org.springframework.test.context.ContextConfiguration;
import org.springframework.test.context.TestPropertySource;
import org.springframework.test.context.junit.jupiter.SpringExtension;
import org.springframework.test.web.servlet.MockMvc;
import org.springframework.test.web.servlet.result.MockMvcResultHandlers;

import com.vmware.blockchain.MvcConfig;
import com.vmware.blockchain.auth.AuthenticationContext;
import com.vmware.blockchain.common.ErrorCode;
import com.vmware.blockchain.common.HelenExceptionHandler;
import com.vmware.blockchain.common.NotFoundException;
import com.vmware.blockchain.security.HelenUserDetails;
import com.vmware.blockchain.security.JwtTokenProvider;
import com.vmware.blockchain.utils.ControllerTestConfig;

/**
 * User Authenticator tests.
 * Initialize with MvcConfig, and only scan this component.
 */
@ExtendWith(SpringExtension.class)
@WebMvcTest(secure = false, controllers = UserAuthenticator.class)
@ContextConfiguration(classes = {MvcConfig.class, ControllerTestConfig.class})
@ComponentScan(basePackageClasses = { UserAuthenticatorTest.class, HelenExceptionHandler.class })
@TestPropertySource(properties = "spring.main.allow-bean-definition-overriding=true")
class UserAuthenticatorTest {

    // Just some random UUIDs
    private static final UUID USER_ID = UUID.fromString("f1c1aa4f-4958-4e93-8a51-930d595fb65b");
    private static final UUID ORG_ID = UUID.fromString("82634974-88cf-4944-a99d-6b92664bb765");
    private static final UUID CONSORTIUM_ID = UUID.fromString("5c7cd0e9-57ad-44af-902f-74af2f3dd8fe");

    @Autowired
    private MockMvc mvc;

    @Autowired
    UserService userService;

    @Autowired
    KeystoreService keystoreService;

    @Autowired
    PasswordEncoder passwordEncoder;

    @Autowired
    JwtTokenProvider jwtTokenProvider;

    @Autowired
    ProfilesService prm;

    private User testUser;

    /**
     * initialize test user and mocks.
     */
    @BeforeEach
    void init() {
        Consortium consortium = new Consortium();
        consortium.setId(CONSORTIUM_ID);
        consortium.setConsortiumName("Consortium Test");
        consortium.setConsortiumType("Test Type");
        Organization organization = new Organization();
        organization.setId(ORG_ID);
        organization.setOrganizationName("Test Org");
        // our test user
        testUser = new User();
        testUser.setId(USER_ID);
        testUser.setEmail("user@test.com");
        testUser.setFirstName("Test");
        testUser.setLastName("User");
        testUser.setPassword("1234");
        testUser.setOrganization(ORG_ID);
        testUser.setServiceRoles(Collections.singletonList(VmbcRoles.SYSTEM_ADMIN));

        // The order of these matters.  When the user is "user@test.com" return the test user,
        // otherwise, return and empty optional.
        when(userService.getByEmail(AdditionalMatchers.not(eq("user@test.com"))))
                .thenThrow(new NotFoundException(ErrorCode.NOT_FOUND));
        when(userService.getByEmail("user@test.com")).thenReturn(testUser);
        when(keystoreService.getWalletsForUser(any(UUID.class))).thenReturn(Collections.emptyList());

        when(passwordEncoder.matches(anyString(), anyString())).thenReturn(false);
        when(passwordEncoder.matches("1234", "1234")).thenReturn(true);
        when(passwordEncoder.encode(anyString())).then(a -> a.getArguments().toString());

        HelenUserDetails details = new HelenUserDetails(USER_ID, "user@test.com", "1234",
                                                        Arrays.asList(new SimpleGrantedAuthority("SYSTEM_ADMIN")));

        when(jwtTokenProvider.createToken(any(User.class))).thenReturn("token");
        when(jwtTokenProvider.createRefreshToken(any(User.class))).thenReturn("refresh_token");
        when(jwtTokenProvider.getAuthentication("token"))
            .thenReturn(new AuthenticationContext(details, Collections.emptyList()));
        when(jwtTokenProvider.getValidityInMilliseconds()).thenReturn(1800000L);

        doAnswer(invocation -> {
            User u = invocation.getArgument(0);
            u.setLastLogin(Instant.now());
            return null;
        }).when(prm).loginUser(any(User.class));
    }


    @Test
    void loginTest() throws Exception {
        String loginRequest = "{\"email\": \"user@test.com\", \"password\": \"1234\"}";
        String loginResponse =
                "{\"user_id\":\"f1c1aa4f-4958-4e93-8a51-930d595fb65b\","
                + "\"last_login\":0, \"token\":\"token\",\"refresh_token\":"
                + "\"refresh_token\",\"token_expires\":1800000}";
        mvc.perform(post("/api/auth/login")
                .contentType(MediaType.APPLICATION_JSON).content(loginRequest))
                .andDo(MockMvcResultHandlers.print())
                .andExpect(status().isOk()).andExpect(content().json(loginResponse));
        Assertions.assertNotEquals(0, testUser.getLastLogin().toEpochMilli());
    }

    @Test
    void missingParamTest() throws Exception {
        String loginRequest = "{\"email\": \"user@test.com\"}";
        String loginResponse = "{\"error_message\":\"Invalid email/password\"}";
        mvc.perform(post("/api/auth/login").with(csrf()).with(csrf())
                .contentType(MediaType.APPLICATION_JSON).content(loginRequest))
                .andExpect(status().is(401)).andExpect(content().json(loginResponse));
    }

    @Test
    void badPasswordTest() throws Exception {
        String loginRequest = "{\"email\": \"user@test.com\", \"password\": \"3456\"}";
        String loginResponse = "{\"error_message\":\"Invalid email/password\"}";
        mvc.perform(post("/api/auth/login").with(csrf())
                .contentType(MediaType.APPLICATION_JSON).content(loginRequest))
                .andExpect(status().is(401)).andExpect(content().json(loginResponse));
    }

    @Test
    void noUserTest() throws Exception {
        String loginRequest = "{\"email\": \"baduser@test.com\", \"password\": \"1234\"}";
        String loginResponse = "{\"error_message\":\"Invalid email/password\"}";
        mvc.perform(post("/api/auth/login").with(csrf())
                .contentType(MediaType.APPLICATION_JSON).content(loginRequest))
            .andExpect(status().is(401)).andExpect(content().json(loginResponse));
    }

    @Test
    void tokenTest() throws Exception {
        String tokenRequest = "{\"refresh_token\": \"token\"}";
        String tokenResponse = "{\"refresh_token\":\"refresh_token\",\"token_expires\":1800000,\"token\":\"token\"}";
        mvc.perform(post("/api/auth/token").with(csrf())
                .characterEncoding("utf-8")
                .content(tokenRequest).contentType(MediaType.APPLICATION_JSON))
            .andExpect(status().isOk()).andExpect(content().json(tokenResponse));
    }

}

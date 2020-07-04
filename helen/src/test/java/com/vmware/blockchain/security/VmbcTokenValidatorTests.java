/*
 * Copyright (c) 2019 VMware, Inc. All rights reserved. VMware Confidential
 */

package com.vmware.blockchain.security;

import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.anyString;
import static org.mockito.Mockito.doThrow;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import java.security.KeyPair;
import java.security.KeyPairGenerator;
import java.time.Instant;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Date;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.UUID;
import java.util.function.Consumer;

import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.springframework.security.authentication.BadCredentialsException;
import org.springframework.security.core.GrantedAuthority;
import org.springframework.test.context.ContextConfiguration;
import org.springframework.test.context.TestPropertySource;
import org.springframework.test.context.junit.jupiter.SpringExtension;

import com.vmware.blockchain.BaseCacheHelper;
import com.vmware.blockchain.common.ConflictException;
import com.vmware.blockchain.common.csp.CspCommon.CspUser;
import com.vmware.blockchain.common.csp.CspJwksSigningKeyResolver;
import com.vmware.blockchain.common.csp.api.client.CspApiClient;
import com.vmware.blockchain.services.blockchains.BlockchainService;
import com.vmware.blockchain.services.profiles.OrganizationService;
import com.vmware.blockchain.services.profiles.User;
import com.vmware.blockchain.services.profiles.UserService;
import com.vmware.blockchain.services.profiles.VmbcRoles;
import com.vmware.blockchain.utils.ControllerTestConfig;

import io.jsonwebtoken.Claims;
import io.jsonwebtoken.JwsHeader;
import io.jsonwebtoken.Jwts;
import io.jsonwebtoken.SignatureAlgorithm;

/**
 * Tests CspOrgAuthService.
 */
@ExtendWith(SpringExtension.class)
@TestPropertySource(locations = "classpath:test.properties")
@ContextConfiguration(classes = {ControllerTestConfig.class})
public class VmbcTokenValidatorTests {

    private VmbcTokenValidator cspAuthService;

    private CspApiClient cspApiClient;
    private BaseCacheHelper baseCacheHelper;
    private UserService userService;
    private OrganizationService organizationService;
    private BlockchainService blockchainService;

    private String userName;
    private CspUser user;
    private KeyPair keyPair;
    private ServiceContext serviceContext;

    private CspJwksSigningKeyResolver cspSigningKeyResolver;

    /**
     * Initialize tests. Set up mocks for dependencies.
     */
    @BeforeEach
    public void setup() throws Exception {
        cspApiClient = mock(CspApiClient.class);
        baseCacheHelper = mock(BaseCacheHelper.class);
        cspSigningKeyResolver = mock(CspJwksSigningKeyResolver.class);
        userService = mock(UserService.class);
        organizationService = mock(OrganizationService.class);
        blockchainService = mock(BlockchainService.class);
        serviceContext = mock(ServiceContext.class);

        cspAuthService =
                new VmbcTokenValidator(
                        cspApiClient, cspSigningKeyResolver,
                        baseCacheHelper, userService, organizationService, blockchainService,
                        serviceContext, "serviceId");

        userName = "batman@wayneenterprises.com";
        user = new CspUser();
        user.setFirstName("Bruce");
        user.setLastName("Wayne");
        user.setUsername(userName);
        keyPair = KeyPairGenerator.getInstance("RSA").generateKeyPair();
        User user = new User();
        user.setId(UUID.fromString("f8cf0d8f-27b1-404a-ab0f-664d0913c417"));
        when(userService.getByEmail(anyString())).thenReturn(user);
        when(cspSigningKeyResolver.resolveSigningKey(any(JwsHeader.class), any(Claims.class)))
                .thenReturn(keyPair.getPublic());
    }

    @Test
    public void testGazToken() throws Exception {
        UUID orgId = UUID.randomUUID();
        String authToken = createAuthToken(orgId, null);

        HelenUserDetails userInfo = cspAuthService.validateAndGetAuthz(authToken);
        Assertions.assertNotNull(userInfo);
        Assertions.assertEquals("vmc_testuser2_dev", userInfo.getUsername());
        Assertions.assertEquals(orgId, userInfo.getOrgId());
        // VB-1630: Update chains was null
        Assertions.assertNotNull(userInfo.getAccessChains());
        Assertions.assertNotNull(userInfo.getUpdateChains());
        Assertions.assertNotNull(userInfo.getAccessConsortiums());
        Assertions.assertNotNull(userInfo.getUpdateConsortiums());
        List<GrantedAuthority> expectedRoles = Arrays.asList(VmbcRoles.CSP_ORG_OWNER, VmbcRoles.ORG_USER);
        List<GrantedAuthority> actualRoles = new ArrayList<GrantedAuthority>(userInfo.getAuthorities());
        Assertions.assertEquals(expectedRoles, actualRoles);

        verify(cspApiClient, never()).getUser(any());
    }

    @Test
    public void testCantUpdateLogin() throws Exception {
        doThrow(new ConflictException("conflict")).when(userService).merge(any(User.class), any(Consumer.class));
        UUID orgId = UUID.randomUUID();
        String authToken = createAuthToken(orgId, null);

        HelenUserDetails userInfo = cspAuthService.validateAndGetAuthz(authToken);
        Assertions.assertNotNull(userInfo);
        Assertions.assertEquals("vmc_testuser2_dev", userInfo.getUsername());
        Assertions.assertEquals(orgId, userInfo.getOrgId());
        List<GrantedAuthority> expectedRoles = Arrays.asList(VmbcRoles.CSP_ORG_OWNER, VmbcRoles.ORG_USER);
        List<GrantedAuthority> actualRoles = new ArrayList<GrantedAuthority>(userInfo.getAuthorities());
        Assertions.assertEquals(expectedRoles, actualRoles);

        verify(cspApiClient, never()).getUser(any());
    }

    /**
     * CSP sends a context_name even when not logged in to org. useVidm = True
     */
    @Test
    public void testGazDefaultOrgToken() throws Exception {
        Map<String, Object> claims = new HashMap<>();
        claims.put("acct", "vmc_testuser2_dev");
        claims.put("context_name", "default");
        String authToken = Jwts.builder()
                .signWith(SignatureAlgorithm.RS256, keyPair.getPrivate())
                .setSubject("unit test")
                .setClaims(claims)
                .compact();

        HelenUserDetails userInfo = cspAuthService.validateAndGetAuthz(authToken);
        Assertions.assertNotNull(userInfo);
        verify(cspApiClient, never()).getUser(any());
    }


    @Test
    public void testBadSig() throws Exception {

        final KeyPair keyPair2 = KeyPairGenerator.getInstance("RSA").generateKeyPair();
        Map<String, Object> claims = new HashMap<>();
        claims.put("username", "vmc_testuser2_dev");
        String authToken = Jwts.builder()
                .signWith(SignatureAlgorithm.RS256, keyPair2.getPrivate())
                .setSubject("unit test")
                .setClaims(claims)
                .compact();

        Assertions.assertThrows(BadCredentialsException.class,
            () -> cspAuthService.validateAndGetAuthz(authToken));
    }

    /**
     * Test random authToken. Make sure with vidm=True we don't fall into old code.
     */
    @Test
    public void testInvalidToken() throws Exception {
        String authToken = UUID.randomUUID().toString();
        Assertions.assertThrows(BadCredentialsException.class,
            () -> cspAuthService.validateAndGetAuthz(authToken));
        verify(cspApiClient, never()).getUser(any());
    }

    /**
     * Test that we log the username when we encounter an expired token.
     */
    @Test
    public void testExpiredToken() throws Exception {
        UUID orgId = UUID.randomUUID();
        String authToken = createAuthToken(orgId, Date.from(Instant.now().minusSeconds(3600)));

        BadCredentialsException e = Assertions.assertThrows(BadCredentialsException.class,
            () -> cspAuthService.validateAndGetAuthz(authToken));
        Assertions.assertTrue(e.getMessage().contains("vmc_testuser2_dev"));
    }

    private String createAuthToken(UUID orgId, Date expires) {
        if (expires == null) {
            expires = Date.from(Instant.now().plusSeconds(3600));
        }
        Map<String, Object> claims = new HashMap<>();
        claims.put("context_name", orgId.toString());
        claims.put("acct", "vmc_testuser2_dev");
        claims.put("perms", Arrays.asList("csp:org_owner",
                                          "csp:org_memeber",
                                          "external/serviceId/vmbc-org:user"));
        String authToken = Jwts.builder()
                .signWith(SignatureAlgorithm.RS256, keyPair.getPrivate())
                .setSubject("unit test")
                .setClaims(claims)
                .setExpiration(expires)
                .compact();
        return authToken;
    }

}
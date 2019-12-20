/*
 * Copyright (c) 2019 VMware, Inc. All rights reserved. VMware Confidential
 */

package com.vmware.blockchain.common.csp;

import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

import java.security.PublicKey;

import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.Mock;
import org.springframework.test.context.junit.jupiter.SpringExtension;

import com.google.common.collect.ImmutableMap;
import com.vmware.blockchain.common.InternalFailureException;
import com.vmware.blockchain.common.csp.api.client.CspApiClient;

import io.jsonwebtoken.Claims;
import io.jsonwebtoken.JwsHeader;
import io.jsonwebtoken.impl.DefaultJwsHeader;

/**
 * Tests for the {@link CspJwksSigningKeyResolver}.
 */
@ExtendWith(SpringExtension.class)
public class CspJwksSigningKeyResolverTest {

    @Mock
    private CspApiClient cspApiClient;

    private String keyId1 = "super_duper_key";
    private String keyId2 = "super_than_super_duper_key";

    @Test
    public void testInit() throws Exception {
        PublicKey key1Pk = mock(PublicKey.class);
        PublicKey key2Pk = mock(PublicKey.class);
        when(cspApiClient.getJwtPublicKeys()).thenReturn(ImmutableMap.of(keyId1, key1Pk, keyId2, key2Pk));
        new CspJwksSigningKeyResolver(cspApiClient);
    }

    @Test
    public void testKeyResolution() throws Exception {
        PublicKey key1Pk = mock(PublicKey.class);
        PublicKey key2Pk = mock(PublicKey.class);

        when(cspApiClient.getJwtPublicKeys()).thenReturn(ImmutableMap.of(keyId1, key1Pk, keyId2, key2Pk));

        CspJwksSigningKeyResolver signingKeyResolver = new CspJwksSigningKeyResolver(cspApiClient);

        Assertions.assertEquals(key1Pk, signingKeyResolver
                .resolveSigningKey(new DefaultJwsHeader(ImmutableMap.of(JwsHeader.KEY_ID, keyId1)),
                                   mock(Claims.class)));
        Assertions.assertEquals(key2Pk, signingKeyResolver
                .resolveSigningKey(new DefaultJwsHeader(ImmutableMap.of(JwsHeader.KEY_ID, keyId2)),
                                   mock(Claims.class)));
    }

    @Test
    public void testKeyResolutionInvalidKey() throws Exception {
        PublicKey key1Pk = mock(PublicKey.class);
        when(cspApiClient.getJwtPublicKeys()).thenReturn(ImmutableMap.of(keyId1, key1Pk));

        InternalFailureException e = Assertions.assertThrows(InternalFailureException.class, () -> {
            CspJwksSigningKeyResolver signingKeyResolver =
                    new CspJwksSigningKeyResolver(cspApiClient);
            Assertions.assertEquals(key1Pk, signingKeyResolver
                    .resolveSigningKey(
                            new DefaultJwsHeader(ImmutableMap.of(JwsHeader.KEY_ID, keyId1)),
                            mock(Claims.class)));

            signingKeyResolver.resolveSigningKey(
                    new DefaultJwsHeader(ImmutableMap.of(JwsHeader.KEY_ID, keyId2)),
                    mock(Claims.class));
        });
        Assertions.assertEquals("Invalid Key ID super_than_super_duper_key", e.getMessage());

    }
}
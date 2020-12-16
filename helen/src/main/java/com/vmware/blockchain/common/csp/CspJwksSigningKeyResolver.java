/*
 * Copyright (c) 2019 VMware, Inc. All rights reserved. VMware Confidential
 */

package com.vmware.blockchain.common.csp;

import java.security.Key;
import java.security.PublicKey;
import java.util.Optional;
import java.util.concurrent.ConcurrentHashMap;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import com.vmware.blockchain.common.ErrorCodeType;
import com.vmware.blockchain.common.InternalFailureException;
import com.vmware.blockchain.common.csp.api.client.CspApiClient;

import io.jsonwebtoken.Claims;
import io.jsonwebtoken.JwsHeader;
import io.jsonwebtoken.SigningKeyResolver;
import io.jsonwebtoken.UnsupportedJwtException;
import lombok.extern.slf4j.Slf4j;


/**
 * An implementation of {@link SigningKeyResolver} for resolving the key based on the kid (KeyId) on the jwt header,
 * with a cache of keys from CSP.
 */
@Slf4j
@Service
public class CspJwksSigningKeyResolver implements SigningKeyResolver {

    private ConcurrentHashMap<String, PublicKey> signingKeyMap;

    private PublicKey defaultKey;

    private CspApiClient cspApiClient;

    /**
     * A {@link SigningKeyResolver} for csp. https://confluence.eng.vmware.com/x/cll5Fg More information here:
     *
     * @param cspApiClient - An instance of {@link CspApiClient}
     * @throws Exception - if unable to fetch keys from CSP.
     */
    @Autowired
    public CspJwksSigningKeyResolver(CspApiClient cspApiClient) throws Exception {
        this.cspApiClient = cspApiClient;
        this.signingKeyMap = new ConcurrentHashMap<>(cspApiClient.getJwtPublicKeys());
        this.defaultKey = cspApiClient.getJwtPublicKey();
    }

    @Override
    public Key resolveSigningKey(JwsHeader header, Claims claims) {
        if (header.getKeyId() == null) {
            return defaultKey;
        }
        return getSigningKey(header.getKeyId())
                .orElseThrow(() -> new InternalFailureException(
                        ErrorCodeType.CSP_INVALID_JWT_KEY_ID, header.getKeyId()
                ));
    }

    @Override
    public Key resolveSigningKey(JwsHeader header, String plaintext) {
        throw new UnsupportedJwtException("Plaintext JWS is not supported");
    }

    private synchronized Optional<PublicKey> getSigningKey(String kid) {
        PublicKey publicKey = signingKeyMap.get(kid);
        if (publicKey != null) {
            return Optional.of(publicKey);
        }
        log.info("Public key not present in cache for kid {}, reloading keymap, existing keys {} ", kid,
                 signingKeyMap.keySet());
        try {
            this.signingKeyMap = new ConcurrentHashMap<>(cspApiClient.getJwtPublicKeys());
            log.info("Public key not present in cache for kid {}, reloading keymap, existing keys {} ", kid,
                     signingKeyMap.keySet());
        } catch (Exception e) {
            log.error("Exception while trying to load keys", e);
        }
        return Optional.ofNullable(signingKeyMap.get(kid));
    }
}

/*
 * Copyright (c) 2018-2019 VMware, Inc. All rights reserved. VMware Confidential
 */

package com.vmware.blockchain.security;

import java.util.Base64;
import java.util.Date;
import java.util.List;
import java.util.Objects;
import java.util.Random;
import java.util.UUID;
import java.util.stream.Collectors;

import javax.annotation.PostConstruct;
import javax.servlet.http.HttpServletRequest;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.cache.annotation.Cacheable;
import org.springframework.security.core.Authentication;
import org.springframework.security.core.GrantedAuthority;
import org.springframework.security.core.authority.SimpleGrantedAuthority;
import org.springframework.stereotype.Component;

import com.vmware.blockchain.auth.AuthenticationContext;
import com.vmware.blockchain.common.Constants;
import com.vmware.blockchain.common.ErrorCode;
import com.vmware.blockchain.common.UnauthorizedException;
import com.vmware.blockchain.services.blockchains.BlockchainService;
import com.vmware.blockchain.services.profiles.Organization;
import com.vmware.blockchain.services.profiles.OrganizationService;
import com.vmware.blockchain.services.profiles.User;
import com.vmware.blockchain.services.profiles.UserService;

import io.jsonwebtoken.Claims;
import io.jsonwebtoken.JwtException;
import io.jsonwebtoken.Jwts;
import io.jsonwebtoken.SignatureAlgorithm;
import lombok.Getter;

/**
 * Bean to provide JWT tokens.  Creates, refreshes and signs tokens.
 */
@Component
public class JwtTokenProvider {
    // Key length in bytes.  Recommendation for HS256 is 256 bits (32 bytes)
    static final int KEY_LENGTH = 32;

    @Value("${security.jwt.token.secretkey:#{null}}")
    private String secretKey;

    @Getter
    @Value("${security.jwt.token.expire-length:1800000}")
    private long validityInMilliseconds = 1800000; // 30 minutes

    @Value("${security.jwt.token.refresh.expire-length:3600000}")
    private long refreshTokenValidityInMilliseconds = 3600000; // 1 hour

    @Autowired
    private HelenUserDetailsService helenUserDetailsService;

    @Autowired
    private OrganizationService organizationService;

    @Autowired
    private UserService userService;

    @Autowired
    private BlockchainService blockchainService;

    @PostConstruct
    protected void init() {
        if (secretKey == null) {
            // Generate a random secret key 256 bits long
            byte[] secret = new byte[KEY_LENGTH];
            // SecureRandom hangs in normal configuration on unix systems.  This is not as
            // strong, but will be fine for our purposes.
            new Random().nextBytes(secret);
            secretKey = Base64.getEncoder().encodeToString(secret);
        }
    }

    /**
     * Create a new JWT with the specified user name and roles.
     */
    public String createToken(User user) {
        return createJwt(user, validityInMilliseconds);
    }

    /**
     * Create a refresh token for given user with roles.
     */
    public String createRefreshToken(User user) {
        return createJwt(user, refreshTokenValidityInMilliseconds);
    }

    private String createJwt(User user, long ttl) {
        Claims claims = Jwts.claims().setSubject(user.getEmail());
        // Changing this to be more like GAZ generated token
        claims.put("perms", user.getServiceRoles().stream().map(s -> s.getAuthority())
                .filter(Objects::nonNull).collect(Collectors.toList()));
        // "context_name" is what this field will be when we integrate with CSP
        claims.put("context_name", user.getOrganization());

        Date now = new Date();
        Date validity = new Date(now.getTime() + ttl);

        return Jwts.builder().setClaims(claims).setIssuedAt(now).setExpiration(validity)
                .signWith(SignatureAlgorithm.HS256, secretKey).compact();
    }

    /**
     * Return an Authentication for the give token.  Note that this always returns a value.
     * Throws HelenException if anything is wrong.
     */
    @Cacheable(Constants.TOKEN_CACHE)
    public Authentication getAuthentication(String token) {
        // throws exception if token not valid
        Claims claims = validateToken(token);
        String email = claims.getSubject();
        String orgId = claims.get("context_name", String.class);
        @SuppressWarnings("unchecked")
        List<String> roles = claims.get("perms", List.class);
        final List<GrantedAuthority> authorities =
                roles.stream().map(r -> new SimpleGrantedAuthority(r)).collect(Collectors.toList());
        // throws exception if user not found.
        HelenUserDetails userDetails = (HelenUserDetails) helenUserDetailsService.loadUserByUsername(email);
        Organization o = organizationService.get(UUID.fromString(orgId));
        userDetails.setAuthToken(token);
        userDetails.setOrgId(UUID.fromString(orgId));
        List<UUID> ids =
                organizationService.getConsortiums(o.getId()).stream()
                        .map(blockchainService::listByConsortium)
                        .flatMap(c -> c.stream())
                        .map(b -> b.getId()).distinct().collect(Collectors.toList());
        userDetails.setAccessChains(ids);

        return new AuthenticationContext(userDetails,  authorities);
    }

    /**
     * Pull the Bearer token from Authorization field in the request header.
     */
    public String resolveToken(HttpServletRequest req) {
        String bearerToken = req.getHeader("Authorization");
        if (bearerToken != null && bearerToken.startsWith("Bearer ")) {
            return bearerToken.substring(7, bearerToken.length());
        }
        return null;
    }

    /**
     * Check the validity of the current token, and get the claims.
     */
    private Claims validateToken(String token) {
        try {
            return Jwts.parser().setSigningKey(secretKey).parseClaimsJws(token).getBody();
        } catch (JwtException | IllegalArgumentException e) {
            throw new UnauthorizedException(ErrorCode.INVALID_JWT_TOKEN);
        }
    }


}

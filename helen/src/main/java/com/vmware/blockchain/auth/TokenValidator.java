/*
 * Copyright (c) 2019 VMware, Inc. All rights reserved. VMware Confidential
 */

package com.vmware.blockchain.auth;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.security.authentication.BadCredentialsException;

import com.vmware.blockchain.security.HelenUserDetails;

import io.jsonwebtoken.Claims;
import io.jsonwebtoken.ExpiredJwtException;
import io.jsonwebtoken.Jws;
import io.jsonwebtoken.JwtParser;
import io.jsonwebtoken.Jwts;
import io.jsonwebtoken.SigningKeyResolver;
import io.jsonwebtoken.UnsupportedJwtException;

/**
 * Interface to abstract out mapping an authToken to appropriate orgs and roles.
 * For the most part this is a username, and an orgId -> roles mapping - if the authtoken contains those.
 * For vIDM/GAZ the token will contain an org and roles if the user is in the context of an org.
 * For DI this implementation has to figure out which orgs the caller has and fake roles.
 */

public interface TokenValidator {

    /**
     * In cases where API calls are made across tenant (impersonation, operator) we need to
     * fetch some org-related data that is put into the authentication context.
     */
    Logger logger = LoggerFactory.getLogger(TokenValidator.class);
    JwtParser jwtParser = Jwts.parser();
    JwtParser jwksJwtParser = Jwts.parser();

    /**
     * Take an (CSP) authToken and return information that TokenAuthenticationProvider requires.
     * @param token authToken produces by caller.
     */
    HelenUserDetails validateAndGetAuthz(String token);


    /**
     * A basic helper function that parses a CSP JWT.
     *
     * @param token              an authToken
     * @param signingKeyResolver CSP signing key resolver for JWT
     */
    default Jws<Claims> parseJwt(String token, SigningKeyResolver signingKeyResolver) {
        Jws<Claims> parsedToken;
        try {
            jwksJwtParser.setSigningKeyResolver(signingKeyResolver);
            parsedToken = jwksJwtParser.parseClaimsJws(token);
        } catch (ExpiredJwtException expired) {
            String userName = expired.getClaims().get("acct", String.class);
            logger.info("Expired token for {}", userName);
            throw new BadCredentialsException("Expired token for " + userName);
        } catch (UnsupportedJwtException ue) {
            // hack - re throw this so we continue with old path.
            // This is usually for API key logins (which we need to get rid of!)
            // But of course is also for bad/unknown/fake authTokens.
            throw ue;
        } catch (Exception ex) {
            logger.info("Error decoding JWT {}: {}", ex.getMessage(), ex.toString());
            throw new BadCredentialsException("Auth token is not valid");
        }
        return parsedToken;
    }
}

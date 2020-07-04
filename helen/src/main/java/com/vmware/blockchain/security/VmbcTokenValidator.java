/*
 * Copyright (c) 2019 VMware, Inc. All rights reserved. VMware Confidential
 */

package com.vmware.blockchain.security;

import java.time.Instant;
import java.util.ArrayList;
import java.util.List;
import java.util.Objects;
import java.util.UUID;
import java.util.stream.Collectors;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.apache.logging.log4j.message.ParameterizedMessage;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.cache.annotation.Cacheable;
import org.springframework.security.authentication.BadCredentialsException;
import org.springframework.stereotype.Component;

import com.vmware.blockchain.BaseCacheHelper;
import com.vmware.blockchain.auth.TokenValidator;
import com.vmware.blockchain.base.auth.Role;
import com.vmware.blockchain.common.Constants;
import com.vmware.blockchain.common.NotFoundException;
import com.vmware.blockchain.common.csp.CspCommon.CspOrg;
import com.vmware.blockchain.common.csp.CspCommon.CspUser;
import com.vmware.blockchain.common.csp.CspConstants;
import com.vmware.blockchain.common.csp.CspJwksSigningKeyResolver;
import com.vmware.blockchain.common.csp.api.client.CspApiClient;
import com.vmware.blockchain.services.blockchains.BlockchainService;
import com.vmware.blockchain.services.profiles.Organization;
import com.vmware.blockchain.services.profiles.OrganizationService;
import com.vmware.blockchain.services.profiles.User;
import com.vmware.blockchain.services.profiles.UserService;
import com.vmware.blockchain.services.profiles.VmbcRoles;

import io.jsonwebtoken.Claims;
import io.jsonwebtoken.Jws;
import io.jsonwebtoken.SigningKeyResolver;
import lombok.NonNull;

/**
 * Token validator for Helen.
 * Currently contains a kludge to create users and orgs the first time they are encountered.
 */
@Component
public class VmbcTokenValidator implements TokenValidator {
    private static final Logger logger = LogManager.getLogger(VmbcTokenValidator.class);

    private CspApiClient cspApiClient;
    private UserService userService;
    private OrganizationService organizationService;
    private BlockchainService blockchainService;

    private BaseCacheHelper baseCacheHelper;
    private SigningKeyResolver signingKeyResolver;
    private ServiceContext serviceContext;

    private String serviceId;

    @Autowired
    public VmbcTokenValidator(CspApiClient cspApiClient,
                              CspJwksSigningKeyResolver cspSigningKeyResolver,
                              BaseCacheHelper baseCacheHelper,
                              UserService userService,
                              OrganizationService organizationService,
                              BlockchainService blockchainService,
                              ServiceContext serviceContext,
                              @Value("${vmbc.service.id:}") String serviceId) {

        this.signingKeyResolver = cspSigningKeyResolver;
        this.cspApiClient = cspApiClient;
        this.userService = userService;
        this.organizationService = organizationService;
        this.blockchainService = blockchainService;
        this.baseCacheHelper = baseCacheHelper;
        this.serviceContext = serviceContext;
        this.serviceId = serviceId;
    }

    /** Validate the new Refresh(GAZ) token.
     *  Parses the JWT auth token and retrieves the userInfo with OrgId.
     *  Token without org and roles are ignored and just validates the userId from the token
     *  against CSP and returns the userName without any Org & Roles info.
     * @param token authToken (expecting JWT token).
     * @return HelenUserDetails Logged in user info.
     */
    @Override
    @Cacheable(Constants.CSP_TOKEN_CACHE)
    public HelenUserDetails validateAndGetAuthz(String token) {
        try {
            HelenUserDetails userInfo;
            Jws<Claims> claimsJws = parseJwt(token, signingKeyResolver);
            userInfo = getUserInfo(claimsJws);
            userInfo.setAuthToken(token);


            // Get the userid for this user
            User u = getOrCreateUser(userInfo, token, claimsJws);
            userInfo.setUserId(u.getId());
            userInfo.setLastLogin(u.getLastLogin());
            updateLogin(u);

            List<UUID> consortiums =
                    organizationService.getConsortiums(userInfo.getOrgId()).stream()
                            .map(c -> c.getId()).collect(Collectors.toList());
            userInfo.setAccessConsortiums(consortiums);

            // to update a consortium, we must be a member of the consortium org.
            List<UUID> updateConsortiums =
                    organizationService.getConsortiums(userInfo.getOrgId()).stream()
                            .filter(c -> c.getOrganization().equals(userInfo.getOrgId()))
                            .map(c -> c.getId()).collect(Collectors.toList());
            userInfo.setUpdateConsortiums(updateConsortiums);

            List<UUID> chains =
                    organizationService.getConsortiums(userInfo.getOrgId()).stream()
                            .map(blockchainService::listByConsortium)
                            .flatMap(c -> c.stream())
                            .map(b -> b.getId()).distinct().collect(Collectors.toList());
            userInfo.setAccessChains(chains);

            List<UUID> updateChains =
                    organizationService.getConsortiums(userInfo.getOrgId()).stream()
                            .filter(c -> c.getOrganization().equals(userInfo.getOrgId()))
                            .map(blockchainService::listByConsortium)
                            .flatMap(c -> c.stream())
                            .map(b -> b.getId()).distinct().collect(Collectors.toList());
            userInfo.setUpdateChains(updateChains);
            // Check for presence of OrgId from the GAZ auth Token.
            logger.debug("Org {} in jwt for the user {}", userInfo.getOrgId(), userInfo.getUsername());
            return userInfo;
        } catch (BadCredentialsException aex) {
            // already has been logged - just re-throw.
            throw aex;
        } catch (Exception ex) {
            logger.info("Exception processing authToken: {}", ex.toString());
            throw new BadCredentialsException("Auth token is not valid");
        }
    }

    HelenUserDetails parseCspToken(String token, SigningKeyResolver keyResolver) {
        Jws<Claims> claimsJws = parseJwt(token, keyResolver);
        return getUserInfo(claimsJws);
    }


    /**
     * Method to extract {@link HelenUserDetails} from a parsed claim set.
     * @param parsedToken - The parsed token claims.
     * @return - {@link HelenUserDetails} if the claims are valid.
     */
    HelenUserDetails getUserInfo(Jws<Claims> parsedToken) {
        // Use the new information in the CSP JWT
        // tokens may or may not be org-scoped (and therefore may or may not have roles).
        // as explained above - tokens from Api Keys won't parse correctly due to signature.

        String userName = parsedToken.getBody().get("acct", String.class);
        String userId = parsedToken.getBody().get("sub", String.class);

        // Parse context_name - this is either an orgId or 'default'
        String contextName = parsedToken.getBody().get("context_name", String.class);
        UUID orgId = null;
        try {
            orgId = UUID.fromString(contextName);
        } catch (Exception ex) {
            // sometimes CSP passes things like 'default' - we ignore that
            logger.info("Context_name {} for user {} ignored", contextName, userName);
        }

        // Grab permissions/roles.
        List<Role> roles = new ArrayList<>();
        if (parsedToken.getBody().containsKey("perms")) {
            List<Role> permissions = getRoles(parsedToken);
            roles.addAll(permissions);
        }

        HelenUserDetails details =
                new HelenUserDetails(null, userName, roles);
        details.setOrgId(orgId);
        details.setAuthToken("");
        return details;
    }

    @NonNull
    private List<Role> getRoles(Jws<Claims> parsedToken) {
        @SuppressWarnings("unchecked")
        ArrayList<String> perms = (ArrayList<String>) parsedToken.getBody().get("perms");
        // strip off the role prefix, if it's there, than map the role name to our roles.
        // The only non-service role we want is csp:org-owner
        String rolePrefix = CspConstants.CSP_VMBC_ROLE_PREFIX
                            + serviceId + "/";
        List<Role> roles = perms.stream()
                .map(r -> r.startsWith(rolePrefix) ?  r.substring(rolePrefix.length()) : r)
                .map(n -> VmbcRoles.get(n))
                .filter(Objects::nonNull)
                .collect(Collectors.toList());
        // Spring uses Role system admln.
        if (roles.contains(VmbcRoles.SYSTEM_ADMIN)) {
            roles.add(VmbcRoles.ROLE_SYSTEM_ADMIN);
        }
        return roles;
    }


    /*
        Putting this in place as a temporary workaround.  When someone uses an auth token for the first time,
        Create the user and ofganization entries if needed.
        TODO: Find a better way to do this.
     */
    private User getOrCreateUser(HelenUserDetails userInfo, String token, Jws<Claims> parsedToken) {
        // Make sure the org exists.  If a user is a member of multiple orgs, the user may already exist,
        // but org may not.
        Organization org = forceGetOrg(userInfo.getOrgId(), token);
        try {
            return userService.getByEmail(userInfo.getUsername());
        } catch (NotFoundException e) {
            // Hack for now.  User has been authenticated, but doesn't exist in our system yet.
            // Get the user info from CSP


            CspUser cspUser = cspApiClient.getUser(token);
            User user = User.builder().email(userInfo.getUsername())
                    .firstName(cspUser.getFirstName())
                    .lastName(cspUser.getLastName())
                    .password(cspUser.getPassword())
                    .serviceRoles(getRoles(parsedToken))
                    .organization(org.getId()).build();
            serviceContext.setSystemContext();
            return userService.put(user);
        } finally {
            serviceContext.clearServiceContext();
        }
    }

    private Organization forceGetOrg(UUID orgId, String token) {
        try {
            return organizationService.get(orgId);
        } catch (NotFoundException e) {
            CspOrg cspOrg = cspApiClient.getCspOrg(orgId.toString(), token);
            Organization org = new Organization();
            org.setId(orgId);
            org.setOrganizationName(cspOrg.getDisplayName());
            serviceContext.setSystemContext();
            return organizationService.put(org);
        } finally {
            serviceContext.clearServiceContext();
        }
    }

    private void updateLogin(User user) {
        try {
            serviceContext.setSystemContext();
            user.setLastLogin(Instant.now());
            userService.merge(user, m -> {
                m.setLastLogin(user.getLastLogin());
            });
        } catch (Exception e) {
            // If anything goes wrong, log it, but don't blow up authorization
            logger.warn(new ParameterizedMessage("Could not update last login for {}", user), e);
        }
        finally {
            serviceContext.clearServiceContext();
        }
    }

}

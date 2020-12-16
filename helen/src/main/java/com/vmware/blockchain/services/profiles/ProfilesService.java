/*
 * Copyright (c) 2018-2019 VMware, Inc. All rights reserved. VMware Confidential
 */

package com.vmware.blockchain.services.profiles;

import java.util.UUID;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.cache.CacheManager;
import org.springframework.stereotype.Service;

import com.vmware.blockchain.common.BadRequestException;
import com.vmware.blockchain.common.ErrorCodeType;


/**
 * This class manages all persistence related operations related to User management API.
 */
@Service
public class ProfilesService {

    private static Logger logger = LogManager.getLogger(ProfilesService.class);

    @Autowired
    private UserService userService;

    @Autowired
    private OrganizationService organizationService;

    @Autowired
    private ConsortiumService consortiumService;

    @Autowired
    CacheManager cacheManager;


    /* Needed for spring */
    protected ProfilesService() {
    }

    // Make sure the uuid is good.  Throw bad request if not.
    private UUID uuidToString(String strId) {
        try {
            return UUID.fromString(strId);
        } catch (IllegalArgumentException e) {
            throw new BadRequestException(ErrorCodeType.BAD_UUID_FORMAT, e, strId);
        }
    }

    /**
     * Get the user with the given ID.
     * @param userId ID
     * @return User
     */
    public User getUserWithId(String userId) {
        UUID uId = uuidToString(userId);
        return userService.get(uId);
    }

    /**
     * Get the user's consortium ID with email.
     * Note: this should really return a list.  This is another example of
     * where in our system we assume only one org and one cosortium
     * @param email user's email address
     * @return UUID of consortium
     */
    public UUID getUserConsortiumIdWithEmail(String email) {
        User user = userService.getByEmail(email);
        Consortium c = userService.getDefaultConsortium(user);
        return c == null ? null : c.getId();
    }

    /**
     * Get user's organization ID with email.
     * @param email user's email address
     * @return UUID of organization
     */
    public UUID getUserOrganizationIdWithEmail(String email) {
        User user = userService.getByEmail(email);
        return userService.getDefaultOrganization(user).getId();
    }

    /**
     * Utility to return a UsersGetResponse from a user.
     */
    public UsersGetResponse getReponse(User user) {
        Consortium c = userService.getDefaultConsortium(user);
        Organization o = userService.getDefaultOrganization(user);
        UsersGetResponse response = UsersGetResponse.builder()
                .userId(user.getId())
                .name(user.getName())
                .email(user.getEmail())
                .role(user.getServiceRoles().get(0).toString())
                .consortium(c == null ? null : new ConsortiumData(c.getId(), c.getConsortiumName()))
                .organization(new OrganizationData(o.getId(), o.getOrganizationName()))
                .details(new Details(user.getFirstName(), user.getLastName()))
                .lastLogin(user.getLastLogin() == null ? 0 : user.getLastLogin().toEpochMilli())
                .build();
        return response;

    }

}

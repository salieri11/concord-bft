/*
 * Copyright (c) 2018-2019 VMware, Inc. All rights reserved. VMware Confidential
 */

package com.vmware.blockchain.services.profiles;

import java.time.Instant;
import java.util.Collections;
import java.util.List;
import java.util.Optional;
import java.util.UUID;
import java.util.function.Consumer;
import java.util.stream.Collectors;

import javax.transaction.Transactional;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.cache.Cache;
import org.springframework.cache.CacheManager;
import org.springframework.security.crypto.password.PasswordEncoder;
import org.springframework.stereotype.Service;

import com.vmware.blockchain.common.BadRequestException;
import com.vmware.blockchain.common.EntityModificationException;
import com.vmware.blockchain.common.ErrorCode;
import com.vmware.blockchain.common.NotFoundException;



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
    private PasswordEncoder passwordEncoder;

    @Autowired
    CacheManager cacheManager;


    /* Needed for spring */
    protected ProfilesService() {}

    // Make sure the uuid is good.  Throw bad request if not.
    private UUID uuidToString(String strId) {
        try {
            return UUID.fromString(strId);
        } catch (IllegalArgumentException e) {
            throw new BadRequestException(e, ErrorCode.BAD_UUID_FORMAT, strId);
        }
    }

    private List<User> getUsersInternal(String consortiumId, String organizationId) {
        UUID oId = uuidToString(organizationId);
        UUID cId = uuidToString(consortiumId);

        // get the list of organzation ids for this consortium.  Note that we don't really
        // need to check if  it exists.  If it doesn't, this call returns an empty list
        List<UUID> orgIds =
                consortiumService.getOrganizations(cId).stream().map(o -> o.getId()).collect(Collectors.toList());
        if (orgIds.isEmpty() || !orgIds.contains(oId)) {
            return Collections.emptyList();
        }
        return organizationService.getUsers(oId);
    }

    private List<User> getUsersInternalByConsortiumId(String consortiumId) {
        UUID cId = uuidToString(consortiumId);
        return consortiumService.getUsers(cId);
    }

    private List<User> getUsersInternalByOrganizationId(String organizationId) {
        UUID oId = uuidToString(organizationId);
        return organizationService.getUsers(oId);
    }

    /**
     * Get users, optionally filtered on Consortium or Organization.
     * @param consortiumId Optional consortium id
     * @param organizationId Option organization id
     * @return user in JSON object
     */
    public List<User> getUsers(Optional<String> consortiumId, Optional<String> organizationId) {
        List<User> userList;
        if (consortiumId.isPresent() && organizationId.isPresent()) {
            userList = getUsersInternal(consortiumId.get(), organizationId.get());
        } else if (consortiumId.isPresent()) {
            userList = getUsersInternalByConsortiumId(consortiumId.get());
        } else if (organizationId.isPresent()) {
            userList = getUsersInternalByOrganizationId(organizationId.get());
        } else {
            userList = userService.list();
        }
        return userList;

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

    private boolean isDuplicateEmail(String email) {
        try {
            userService.getByEmail(email);
            return true;
        } catch (NotFoundException e) {
            return false;
        }
    }

    /**
     * Patch an existing user. Use the same structure as for create.
     * @param request   User Patch request, with desired changes.
     */
    @Transactional(rollbackOn = Exception.class)
    public void updateUser(UserPatchRequest request) throws EntityModificationException {
        try {
            User user = userService.get(request.getUserId());

            // First check if user with same email already exists
            if (request.getEmail() != null && isDuplicateEmail(request.getEmail())) {
                throw new EntityModificationException(ErrorCode.DUPLICATE_EMAIL);
            }

            ignoreNull(request.getName(), user::setName);
            ignoreNull(request.getEmail(), user::setEmail);
            if (request.getDetails() != null) {
                ignoreNull(request.getDetails().getFirstName(), user::setFirstName);
                ignoreNull(request.getDetails().getLastName(), user::setLastName);
            }
            if (request.getRole() != null) {
                if (Roles.contains(request.getRole())) {
                    user.setRoles(Collections.singletonList(Roles.get(request.getRole())));
                } else {
                    throw new EntityModificationException(ErrorCode.INVALID_ROLE_VALUE + request.getRole());
                }
            }
            userService.put(user);
        } catch (NotFoundException e) {
            throw new EntityModificationException(ErrorCode.USER_ID_NOT_FOUND, request.getUserId());
        }
    }

    public User loginUser(User user) throws EntityModificationException {
        user.setLastLogin(Instant.now());
        return userService.put(user);
    }

    private <T> void ignoreNull(T obj, Consumer<T> c) {
        Optional.ofNullable(obj).ifPresent(c);
    }

    /**
     * Change the user's password.
     */
    public User changePassword(User user, String password) throws EntityModificationException {
        user.setPassword(password);
        user = userService.put(user);
        // if the password changes, we need to evict any cached userDetails
        Cache userCache = cacheManager.getCache("UserCache");
        userCache.evict(user.getEmail());
        return user;
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
                .role(user.getRoles().get(0).toString())
                .consortium(c == null ? null : new ConsortiumData(c.getId(), c.getConsortiumName()))
                .organization(new OrganizationData(o.getId(), o.getOrganizationName()))
                .details(new Details(user.getFirstName(), user.getLastName()))
                .lastLogin(user.getLastLogin() == null ? 0 : user.getLastLogin().toEpochMilli())
                .build();
        return response;

    }

}

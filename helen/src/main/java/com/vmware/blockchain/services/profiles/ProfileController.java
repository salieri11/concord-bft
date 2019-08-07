/*
 * Copyright (c) 2018-2019 VMware, Inc. All rights reserved. VMware Confidential
 */

package com.vmware.blockchain.services.profiles;

import java.io.IOException;
import java.util.List;
import java.util.Optional;
import java.util.UUID;
import java.util.stream.Collectors;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.security.access.prepost.PreAuthorize;
import org.springframework.stereotype.Controller;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestMethod;
import org.springframework.web.bind.annotation.RequestParam;

import com.vmware.blockchain.auth.AuthHelper;
import com.vmware.blockchain.common.EntityModificationException;
import com.vmware.blockchain.common.ErrorCode;
import com.vmware.blockchain.common.ForbiddenException;

import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;

/**
 * A servlet which manages all GET/POST/PATCH requests related to user management API of helen.
 */
@Controller
public class ProfileController {

    private static final long serialVersionUID = 1L;
    private static final Logger logger = LogManager.getLogger(ProfileController.class);

    private ProfilesService prm;
    private UserService userSerivce;
    private DefaultProfiles profiles;
    private AuthHelper authHelper;

    @Autowired
    public ProfileController(ProfilesService prm,
                             UserService userService,
                             DefaultProfiles profiles,
                             AuthHelper authHelper) {
        this.prm = prm;
        this.userSerivce = userService;
        this.profiles = profiles;
        this.authHelper = authHelper;
    }


    /**
     * Get list of users, optionaly filtered on org and consortium.
     */
    @RequestMapping(path = "/api/users", method = RequestMethod.GET)
    @PreAuthorize("@authHelper.isUser()")
    public ResponseEntity<List<UsersGetResponse>> getUsers(
            @RequestParam(name = "consortium", required = false) String consortium,
            @RequestParam(name = "organization", required = false) String organization) {

        List<UsersGetResponse> result =
                prm.getUsers(Optional.ofNullable(consortium), Optional.ofNullable(organization))
                .stream()
                        .filter(u -> authHelper.isSystemAdmin()
                                     || u.getEmail().equals(authHelper.getEmail()))
                        .map(u -> prm.getReponse(u)).collect(Collectors.toList());
        return new ResponseEntity<>(result, HttpStatus.OK);
    }

    /**
     * Get user from ID.
     * @param userId User Id
     * @return the user
     */
    @RequestMapping(path = "/api/users/{user_id}", method = RequestMethod.GET)
    @PreAuthorize("@authHelper.isUserName(#userId)")
    public ResponseEntity<UsersGetResponse> getUserFromId(@PathVariable("user_id") String userId) {
        if (!authHelper.isSystemAdmin()) {
            throw new ForbiddenException(ErrorCode.NOT_ALLOWED);
        }
        return new ResponseEntity<>(prm.getReponse(prm.getUserWithId(userId)), HttpStatus.OK);
    }

    private void defaultUcrFields(UserCreateRequest ucr) {
        if (ucr.getOrganization() == null) {
            ucr.setOrganization(new OrganizationData(profiles.getOrganization().getId(),
                    profiles.getOrganization().getOrganizationName()));
        }
        if (ucr.getConsortium() == null) {
            ucr.setConsortium(new ConsortiumData(profiles.getConsortium().getId(),
                    profiles.getConsortium().getConsortiumName()));
        }
    }

    private void validateCreateRequest(UserCreateRequest ucr) throws EntityModificationException {
        if (ucr.getEmail() == null || ucr.getEmail().isEmpty()) {
            throw new EntityModificationException(ErrorCode.INVALID_EMAIL);
        }
        if (ucr.getName() == null || ucr.getEmail().isEmpty()) {
            throw new EntityModificationException(ErrorCode.INVALID_NAME);
        }
        if (ucr.getPassword() == null || ucr.getPassword().isEmpty()) {
            throw new EntityModificationException(ErrorCode.INVALID_PASSWORD);
        }
    }

    @Data
    @AllArgsConstructor
    @NoArgsConstructor
    private static class UsersCreateResponse {
        UUID userId;
    }

    /**
     * Create a new user.
     */
    @RequestMapping(path = "/api/users", method = RequestMethod.POST)
    @PreAuthorize("isAuthenticated()")
    public ResponseEntity<UsersCreateResponse> doPost(@RequestBody UserCreateRequest ucr) throws IOException {
        if (!authHelper.isSystemAdmin()) {
            throw new ForbiddenException(ErrorCode.NOT_ALLOWED);
        }

        defaultUcrFields(ucr);
        validateCreateRequest(ucr);
        UUID userId = prm.createUser(ucr).getId();
        return new ResponseEntity<>(new UsersCreateResponse(userId), HttpStatus.OK);
    }

    private void validatePatchRequest(UserPatchRequest upr) throws EntityModificationException {
        if (upr.getRole() != null && !Roles.contains(upr.getRole())) {
            throw new EntityModificationException(ErrorCode.INVALID_ROLE);
        }
        if (upr.getDetails() != null) {
            if (upr.getDetails().getLastName() != null && upr.getDetails().getLastName().isEmpty()) {
                throw new EntityModificationException(ErrorCode.INVALID_NAME);
            }
            if (upr.getDetails().getFirstName() != null && upr.getDetails().getFirstName().isEmpty()) {
                throw new EntityModificationException(ErrorCode.INVALID_NAME);
            }
        }
        if (upr.getEmail() != null && upr.getEmail().isEmpty()) {
            throw new EntityModificationException(ErrorCode.INVALID_EMAIL);
        }
        if (upr.getName() != null && upr.getName().isEmpty()) {
            throw new EntityModificationException(ErrorCode.INVALID_NAME);
        }
    }

    /**
     * Update user information.
     */
    @RequestMapping(path = "/api/users/{user_id}", method = RequestMethod.PATCH)
    @PreAuthorize("isAuthenticated()")
    public ResponseEntity<String> doPatch(@PathVariable(name = "user_id") String userId,
            @RequestBody UserPatchRequest upr) {
        if (!authHelper.isSystemAdmin()) {
            throw new ForbiddenException(ErrorCode.NOT_ALLOWED);
        }

        upr.setUserId(UUID.fromString(userId));
        validatePatchRequest(upr);
        prm.updateUser(upr);
        return new ResponseEntity<>("{}", HttpStatus.OK);
    }

}

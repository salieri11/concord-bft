/*
 * Copyright (c) 2018 VMware, Inc. All rights reserved. VMware Confidential
 */

package com.vmware.blockchain.services.profiles;

import java.time.Instant;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.Optional;
import java.util.UUID;
import java.util.stream.Collectors;

import javax.transaction.Transactional;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.json.simple.JSONArray;
import org.json.simple.JSONObject;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.cache.Cache;
import org.springframework.cache.CacheManager;
import org.springframework.security.crypto.password.PasswordEncoder;
import org.springframework.stereotype.Component;

import com.vmware.blockchain.common.EntityModificationException;


/**
 * This class manages all persistence related operations related to User management API.
 */
@Component
@Transactional
public class ProfilesRegistryManager {

    private static Logger logger = LogManager.getLogger(ProfilesRegistryManager.class);

    @Autowired
    private UserRepository userRepository;

    @Autowired
    private OrganizationRepository organizationRepository;

    @Autowired
    private ConsortiumRepository consortiumRepository;

    @Autowired
    private PasswordEncoder passwordEncoder;

    @Autowired
    CacheManager cacheManager;


    /* Needed for spring */
    protected ProfilesRegistryManager() {}

    private List<User> getUsersInternal(String consortiumId, String organizationId) {
        Optional<Organization> org = organizationRepository.findById(UUID.fromString(organizationId));
        Optional<Consortium> con = consortiumRepository.findById(UUID.fromString(consortiumId));

        if (org.isPresent() && con.isPresent()) {
            return userRepository.findUsersByConsortiumAndOrganization(con.get(), org.get());
        } else {
            return Collections.emptyList();
        }
    }

    private List<User> getUsersInternalByConsortiumId(String consortiumId) {
        Optional<Consortium> con = consortiumRepository.findById(UUID.fromString(consortiumId));
        if (con.isPresent()) {
            return new ArrayList<>(con.get().getUsers());
        } else {
            return Collections.emptyList();
        }
    }

    private List<User> getUsersInternalByOrganizationId(String organizationId) {
        Optional<Organization> org = organizationRepository.findById(UUID.fromString(organizationId));
        if (org.isPresent()) {
            return new ArrayList<>(org.get().getUsers());
        } else {
            return Collections.emptyList();
        }
    }

    /**
     * Get users, optionally filtered on Consortium or Organization.
     * @param consortiumId Optional consortium id
     * @param organizationId Option organizartion id
     * @return user in JSON object
     */
    public JSONArray getUsers(Optional<String> consortiumId, Optional<String> organizationId) {
        List<User> userList;
        if (consortiumId.isPresent() && organizationId.isPresent()) {
            userList = getUsersInternal(consortiumId.get(), organizationId.get());
        } else if (consortiumId.isPresent()) {
            userList = getUsersInternalByConsortiumId(consortiumId.get());
        } else if (organizationId.isPresent()) {
            userList = getUsersInternalByOrganizationId(organizationId.get());
        } else {
            userList = userRepository.findAll();
        }

        return userList.stream().map(UsersApiMessage::new).map(UsersGetResponse::toJson).reduce(new JSONArray(),
            (arr, obj) -> {
                arr.add(obj);
                return arr;
            }, (arr1, arr2) -> {
                arr1.addAll(arr2);
                return arr1;
            });
    }

    private List<User> getUsersWithId(List<String> userIdList) {
        return userIdList.stream().map(this::getUserWithIdInternal).filter(Optional::isPresent).map(Optional::get)
                .collect(Collectors.toList());
    }

    private Optional<User> getUserWithIdInternal(String userId) {
        return userRepository.findById(UUID.fromString(userId));
    }

    public JSONObject getUserWithId(String userId) {
        Optional<User> oUser = getUserWithIdInternal(userId);
        return oUser.map(UsersApiMessage::new).map(UsersApiMessage::toJson).orElse(new JSONObject());
    }

    /**
     * Get the user's consortium ID with email.
     * @param email user's email address
     * @return UUID of consortium
     */
    public UUID getUserConsortiumIdWithEmail(String email) {
        Optional<User> oUser = userRepository.findUserByEmail(email);
        if (oUser.isPresent()) {
            User user = oUser.get();
            Consortium consortium = user.getConsortium();
            return consortium.getConsortiumId();
        } else {
            throw new EntityModificationException("No user found with email: " + email);
        }
    }

    /**
     * Get user's organization ID with email.
     * @param email user's email address
     * @return UUID of organization
     */
    public UUID getUserOrganizationIdWithEmail(String email) {
        Optional<User> oUser = userRepository.findUserByEmail(email);
        if (oUser.isPresent()) {
            User user = oUser.get();
            Organization organization = user.getOrganization();
            return organization.getOrganizationId();
        } else {
            throw new EntityModificationException("No user found with email: " + email);
        }
    }

    private boolean isDuplicateEmail(String email) {
        Optional<User> existingUserWithSameEmail = userRepository.findUserByEmail(email);
        return existingUserWithSameEmail.isPresent();
    }

    /**
     * Create a new user.
     */
    public String createUser(UserCreateRequest request) throws EntityModificationException {

        Optional<Organization> o = organizationRepository.findById(request.getOrganizationId());
        Optional<Consortium> c = consortiumRepository.findById(request.getConsortiumId());

        // First check if user with same email already exists
        if (isDuplicateEmail(request.getEmail())) {
            throw new EntityModificationException("Duplicate email address");
        }

        if (o.isPresent() && c.isPresent() && Roles.contains(request.getRole())) {
            User u = new User();
            u.setName(request.getUserName());
            u.setEmail(request.getEmail());
            u.setPassword(passwordEncoder.encode(request.getPassword()));
            u.setRole(Roles.get(request.getRole()));
            u.setOrganization(o.get());
            u.setConsortium(c.get());
            request.getOptionalFirstName().ifPresent(u::setFirstName);
            request.getOptionalLastName().ifPresent(u::setLastName);
            // Note: The order of next 5 statements is very important, The user
            // object must be saved before it can be added and saved into
            // consortium & organization objects.
            u = userRepository.save(u);
            o.get().addUser(u);
            c.get().addUser(u);
            consortiumRepository.save(c.get());
            organizationRepository.save(o.get());
            return u.getUserId().toString();
        } else {
            o.orElseThrow(() -> new EntityModificationException(
                    "Organization with" + " ID " + request.getOrganizationId() + " not found."));
            c.orElseThrow(() -> new EntityModificationException(
                    "Consortium with" + " ID " + request.getConsortiumId() + " not found."));
            throw new EntityModificationException(request.getRole() + " is invalid Role value.");
        }
    }

    /**
     * Patch an existing user.
     */
    public void updateUser(UserPatchRequest request) throws EntityModificationException {
        Optional<User> oUser = userRepository.findById(request.getUserId());

        if (!oUser.isPresent()) {
            throw new EntityModificationException("No user found with ID: " + request.getUserId());
        }

        // First check if user with same email already exists
        if (request.getOptionalEmail().isPresent() && isDuplicateEmail(request.getOptionalEmail().get())) {
            throw new EntityModificationException("Duplicate email address");
        }

        User user = oUser.get();
        request.getOptionalName().ifPresent(user::setName);
        request.getOptionalEmail().ifPresent(user::setEmail);
        request.getOptionalFirstName().ifPresent(user::setFirstName);
        request.getOptionalLastName().ifPresent(user::setLastName);
        if (request.getOptionalRole().isPresent()) {
            if (Roles.contains(request.getOptionalRole().get())) {
                user.setRole(Roles.get(request.getOptionalRole().get()));
            } else {
                throw new EntityModificationException("Invalid role value: " + request.getOptionalRole().get());
            }
        }
        userRepository.save(user);
    }

    /**
     * Login this user.
     */
    public JSONObject loginUser(String email) throws EntityModificationException {
        Optional<User> oUser = userRepository.findUserByEmail(email);
        if (oUser.isPresent()) {
            User u = oUser.get();

            // TODO: We know this is not a long-term solution and this will be
            // replaced by CSP authentication very soon.

            JSONObject userJson = getUserWithId(String.valueOf(u.getUserId()));
            loginUser(u);
            userJson.put("isAuthenticated", Boolean.TRUE);

            return userJson;
        } else {
            throw new EntityModificationException("No user found with email: " + email);
        }
    }

    public void loginUser(User user) throws EntityModificationException {
        user.setLastLogin(Instant.now().toEpochMilli());
        userRepository.save(user);
    }

    /**
     * Change the user's password.
     */
    public JSONObject changePassword(String email, String password) throws EntityModificationException {
        Optional<User> oUser = userRepository.findUserByEmail(email);
        if (oUser.isPresent()) {
            User u = oUser.get();
            final JSONObject userJson = getUserWithId(String.valueOf(u.getUserId()));
            u.setPassword(password);
            u = userRepository.save(u);
            // if the password changes, we need to evict any cached userDetails
            Cache userCache = cacheManager.getCache("UserCache");
            userCache.evict(email);


            return userJson;
        } else {
            throw new EntityModificationException("No user found with email: " + email);
        }
    }


}

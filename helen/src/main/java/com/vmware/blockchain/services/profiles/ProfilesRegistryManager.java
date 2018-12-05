/*
 * Copyright (c) 2018 VMware, Inc. All rights reserved. VMware Confidential
 */

package com.vmware.blockchain.services.profiles;

import java.time.Instant;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.Optional;
import java.util.stream.Collectors;

import javax.transaction.Transactional;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.json.simple.JSONArray;
import org.json.simple.JSONObject;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.context.event.ApplicationReadyEvent;
import org.springframework.cache.Cache;
import org.springframework.cache.CacheManager;
import org.springframework.context.event.EventListener;
import org.springframework.security.crypto.password.PasswordEncoder;
import org.springframework.stereotype.Component;

import com.vmware.blockchain.common.UserModificationException;


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
    private ProfilesRegistryManager prm;

    @Autowired
    private PasswordEncoder passwordEncoder;

    @Autowired
    CacheManager cacheManager;


    /* Needed for spring */
    protected ProfilesRegistryManager() {}

    private List<User> getUsersInternal(String consortiumId, String organizationId) {
        Optional<Organization> org = organizationRepository.findById(Long.parseLong(organizationId));
        Optional<Consortium> con = consortiumRepository.findById(Long.parseLong(consortiumId));

        if (org.isPresent() && con.isPresent()) {
            return userRepository.findUsersByConsortiumAndOrganization(con.get(), org.get());
        } else {
            return Collections.emptyList();
        }
    }

    private List<User> getUsersInternalByConsortiumId(String consortiumId) {
        Optional<Consortium> con = consortiumRepository.findById(Long.parseLong(consortiumId));
        if (con.isPresent()) {
            return new ArrayList<>(con.get().getUsers());
        } else {
            return Collections.emptyList();
        }
    }

    private List<User> getUsersInternalByOrganizationId(String organizationId) {
        Optional<Organization> org = organizationRepository.findById(Long.parseLong(organizationId));
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
        return userRepository.findById(Long.parseLong(userId));
    }

    public JSONObject getUserWithId(String userId) {
        Optional<User> oUser = getUserWithIdInternal(userId);
        return oUser.map(UsersApiMessage::new).map(UsersApiMessage::toJson).orElse(new JSONObject());
    }

    private boolean isDuplicateEmail(String email) {
        Optional<User> existingUserWithSameEmail = userRepository.findUserByEmail(email);
        return existingUserWithSameEmail.isPresent();
    }

    /**
     * Create a new user.
     */
    public String createUser(UserCreateRequest request) throws UserModificationException {

        Optional<Organization> o = organizationRepository.findById(request.getOrganizationId());
        Optional<Consortium> c = consortiumRepository.findById(request.getConsortiumId());

        // First check if user with same email already exists
        if (isDuplicateEmail(request.getEmail())) {
            throw new UserModificationException("Duplicate email address");
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
            return Long.toString(u.getUserId());
        } else {
            o.orElseThrow(() -> new UserModificationException(
                    "Organization with" + " ID " + request.getOrganizationId() + " not found."));
            c.orElseThrow(() -> new UserModificationException(
                    "Consortium with" + " ID " + request.getConsortiumId() + " not found."));
            throw new UserModificationException(request.getRole() + " is invalid Role value.");
        }
    }

    /**
     * Patch an existing user.
     */
    public void updateUser(UserPatchRequest request) throws UserModificationException {
        Optional<User> oUser = userRepository.findById(request.getUserId());

        if (!oUser.isPresent()) {
            throw new UserModificationException("No user found with ID: " + request.getUserId());
        }

        // First check if user with same email already exists
        if (request.getOptionalEmail().isPresent() && isDuplicateEmail(request.getOptionalEmail().get())) {
            throw new UserModificationException("Duplicate email address");
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
                throw new UserModificationException("Invalid role value: " + request.getOptionalRole().get());
            }
        }
        userRepository.save(user);
    }

    /**
     * Login this user.
     */
    public JSONObject loginUser(String email) throws UserModificationException {
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
            throw new UserModificationException("No user found with email: " + email);
        }
    }

    public void loginUser(User user) throws UserModificationException {
        user.setLastLogin(Instant.now().toEpochMilli());
        userRepository.save(user);
    }

    /**
     * Change the user's password.
     */
    public JSONObject changePassword(String email, String password) throws UserModificationException {
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
            throw new UserModificationException("No user found with email: " + email);
        }
    }

    // TODO: These next few methords are just testing convenience methods and should be removed
    // when actual POST API for organization and consortium creation is
    // available

    /**
     * Create the test user if it does not already exist.  Do this when the application goes ready.
     */
    @EventListener(classes = ApplicationReadyEvent.class)
    public void createUserIfNotExist() {
        logger.info("Application ready");
        String email = "admin@blockchain.local";
        String password = "Admin!23";
        List<User> oUser = userRepository.findAll();
        if (oUser.isEmpty()) {
            logger.info("Creating Initial User");
            Organization org = createOrgIfNotExist();
            Consortium consortium = createConsortiumIfNotExist();
            User u = new User();
            u.setName("ADMIN");
            u.setEmail(email);
            u.setPassword(passwordEncoder.encode(password));
            u.setRole(Roles.get("SYSTEM_ADMIN"));
            u.setOrganization(org);
            u.setConsortium(consortium);
            // Note: The order of next 5 statements is very important, The user
            // object must be saved before it can be added and saved into
            // consortium & organization objects.
            u = userRepository.save(u);
            org.addUser(u);
            consortium.addUser(u);
            logger.info("Admin user created. Username: " + email + " Password: " + password);
        }
    }

    /**
     * Create the test org if it doesn't exist.
     */
    public Organization createOrgIfNotExist() {
        List<Organization> oList = organizationRepository.findAll();
        if (oList.isEmpty()) {
            Organization o = new Organization();
            o.setOrganizationName("ADMIN");
            o = organizationRepository.save(o);
            return o;
        } else {
            return oList.get(0);
        }
    }

    /**
     * Create the test consortium if it doesn't exist.
     */
    public Consortium createConsortiumIfNotExist() {
        List<Consortium> cList = consortiumRepository.findAll();
        if (cList.isEmpty()) {
            Consortium c = new Consortium();
            c.setConsortiumName("ADMIN");
            c.setConsortiumType("ADMIN");
            c = consortiumRepository.save(c);
            return c;
        } else {
            return cList.get(0);
        }
    }

}

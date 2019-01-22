/*
 * Copyright (c) 2018-2019 VMware, Inc. All rights reserved. VMware Confidential
 */

package com.vmware.blockchain.services.profiles;

import static com.vmware.blockchain.services.profiles.UsersApiMessage.CONSORTIUM_ID_LABEL;
import static com.vmware.blockchain.services.profiles.UsersApiMessage.CONSORTIUM_LABEL;
import static com.vmware.blockchain.services.profiles.UsersApiMessage.ORGANIZATION_ID_LABEL;
import static com.vmware.blockchain.services.profiles.UsersApiMessage.ORGANIZATION_LABEL;
import static com.vmware.blockchain.services.profiles.UsersApiMessage.USER_ID_LABEL;

import java.io.IOException;
import java.util.Optional;
import java.util.UUID;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.json.simple.JSONArray;
import org.json.simple.JSONAware;
import org.json.simple.JSONObject;
import org.json.simple.parser.JSONParser;
import org.json.simple.parser.ParseException;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.stereotype.Controller;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestMethod;
import org.springframework.web.bind.annotation.RequestParam;

import com.vmware.blockchain.common.ConcordProperties;
import com.vmware.blockchain.common.EntityModificationException;
import com.vmware.blockchain.connections.ConcordConnectionPool;
import com.vmware.blockchain.services.BaseServlet;
import com.vmware.blockchain.services.ethereum.ApiHelper;
import com.vmware.concord.Concord;

/**
 * A servlet which manages all GET/POST/PATCH requests related to user management API of helen.
 */
@Controller
public class ProfileManager extends BaseServlet {

    private static final long serialVersionUID = 1L;
    private static final Logger logger = LogManager.getLogger(ProfileManager.class);

    private ProfilesRegistryManager prm;
    private DefaultProfiles profiles;

    @Autowired
    public ProfileManager(ProfilesRegistryManager prm, DefaultProfiles profiles, ConcordProperties config,
            ConcordConnectionPool concordConnectionPool) {
        super(config, concordConnectionPool);
        this.prm = prm;
        this.profiles = profiles;
    }

    // /api/users?consortium=<c>&organization=<o>
    @RequestMapping(path = "/api/users", method = RequestMethod.GET)
    public ResponseEntity<JSONAware> getUsers(@RequestParam(name = "consortium", required = false) String consortium,
            @RequestParam(name = "organization", required = false) String organization) {
        JSONArray result = prm.getUsers(Optional.ofNullable(consortium), Optional.ofNullable(organization));
        return new ResponseEntity<>(result, standardHeaders, HttpStatus.OK);
    }

    /**
     * Get user from ID.
     * @param userId User Id
     * @return the user
     */
    @RequestMapping(path = "/api/users/{user_id}", method = RequestMethod.GET)
    public ResponseEntity<JSONAware> getUserFromId(@PathVariable("user_id") String userId) {
        JSONObject result = prm.getUserWithId(userId);
        if (result.isEmpty()) {
            return new ResponseEntity<>(new JSONObject(), standardHeaders, HttpStatus.NOT_FOUND);
        } else {
            return new ResponseEntity<>(result, standardHeaders, HttpStatus.OK);
        }
    }

    private void createTestProfiles(JSONObject requestJson) {
        if (!requestJson.containsKey(ORGANIZATION_LABEL)) {
            Organization org = profiles.getOrganization();
            JSONObject orgJson = new JSONObject();
            orgJson.put(ORGANIZATION_ID_LABEL, org.getOrganizationId());
            requestJson.put(ORGANIZATION_LABEL, orgJson);
            logger.debug("New test org created with ID:" + org.getOrganizationId());
        }

        if (!requestJson.containsKey(CONSORTIUM_LABEL)) {
            Consortium consortium = profiles.getConsortium();
            JSONObject consJson = new JSONObject();
            consJson.put(CONSORTIUM_ID_LABEL, consortium.getConsortiumId());
            requestJson.put(CONSORTIUM_LABEL, consJson);
            logger.debug("New test consortium created with ID:" + consortium.getConsortiumId());
        }
    }

    private void validateCreateRequest(UserCreateRequest ucr) throws EntityModificationException {
        if (ucr.getEmail() == null || ucr.getEmail().isEmpty()) {
            throw new EntityModificationException("invalid email specified");
        }
        if (ucr.getUserName() == null || ucr.getEmail().isEmpty()) {
            throw new EntityModificationException("invalid name specified");
        }
        if (ucr.getPassword() == null || ucr.getPassword().isEmpty()) {
            throw new EntityModificationException("invalid password specified");
        }
    }

    /**
     * Create a new user.
     * @param requestBody User info
     * @return user id
     */
    @RequestMapping(path = "/api/users", method = RequestMethod.POST)
    public ResponseEntity<JSONAware> doPost(@RequestBody String requestBody) throws IOException {
        JSONObject responseJson;
        HttpStatus responseStatus;
        try {
            JSONParser parser = new JSONParser();
            JSONObject requestJson = (JSONObject) parser.parse(requestBody);

            // TODO: Ideally the organization and consortium should already exist
            // before adding a USER to that. But for now we just add a new
            // organization & consortium to allow easy testing. Delete this
            // method once testing phase is done
            createTestProfiles(requestJson);
            UsersApiMessage postRequest = new UsersApiMessage(requestJson);

            validateCreateRequest(postRequest);

            String userId = prm.createUser(postRequest);

            responseJson = new JSONObject();
            responseJson.put(USER_ID_LABEL, userId);
            responseStatus = HttpStatus.OK;

        } catch (ParseException | EntityModificationException e) {
            logger.warn("Error while adding new user", e);
            responseJson = ApiHelper.errorJson(e.getMessage());
            responseStatus = HttpStatus.BAD_REQUEST;
        }

        return new ResponseEntity<>(responseJson, standardHeaders, responseStatus);
    }

    private void validatePatchRequest(UserPatchRequest upr) throws EntityModificationException {
        if (upr.getOptionalRole().isPresent() && !Roles.contains(upr.getOptionalRole().get())) {
            throw new EntityModificationException("invalid role provided");
        }
        if (upr.getOptionalLastName().isPresent() && upr.getOptionalLastName().get().isEmpty()) {
            throw new EntityModificationException("invalid last name provided");
        }
        if (upr.getOptionalFirstName().isPresent() && upr.getOptionalFirstName().get().isEmpty()) {
            throw new EntityModificationException("invalid first name provided");
        }
        if (upr.getOptionalEmail().isPresent() && upr.getOptionalEmail().get().isEmpty()) {
            throw new EntityModificationException("invalid email provided");
        }
        if (upr.getOptionalName().isPresent() && upr.getOptionalName().get().isEmpty()) {
            throw new EntityModificationException("invalid name provided");
        }
    }

    /**
     * Update user information.
     * @param userId user id
     * @param requestBody User info to patch
     * @return user id
     */
    @RequestMapping(path = "/api/users/{user_id}", method = RequestMethod.PATCH)
    public ResponseEntity<JSONAware> doPatch(@PathVariable(name = "user_id") String userId,
            @RequestBody String requestBody) {
        HttpStatus responseStatus;
        JSONObject responseJson;
        try {

            JSONParser parser = new JSONParser();
            JSONObject requestJson = (JSONObject) parser.parse(requestBody);
            UserPatchRequest upr = new UsersApiMessage(requestJson);
            upr.setUserId(UUID.fromString(userId));
            validatePatchRequest(upr);
            prm.updateUser(upr);

            responseJson = new JSONObject();
            responseStatus = HttpStatus.OK;

        } catch (ParseException | EntityModificationException e) {
            responseJson = ApiHelper.errorJson(e.getMessage());
            responseStatus = HttpStatus.BAD_REQUEST;
        }

        return new ResponseEntity<>(responseJson, standardHeaders, responseStatus);
    }

    @Override
    protected JSONAware parseToJson(Concord.ConcordResponse concordResponse) {
        throw new UnsupportedOperationException("parseToJSON method is not " + "supported in ProfileManager class");
    }
}

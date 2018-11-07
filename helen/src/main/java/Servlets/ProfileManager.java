/**
 * <p>
 * Copyright 2018 VMware, all rights reserved.
 * </p>
 *
 */

package Servlets;

import static services.profiles.UsersAPIMessage.CONSORTIUM_ID_LABEL;
import static services.profiles.UsersAPIMessage.CONSORTIUM_LABEL;
import static services.profiles.UsersAPIMessage.ORGANIZATION_ID_LABEL;
import static services.profiles.UsersAPIMessage.ORGANIZATION_LABEL;
import static services.profiles.UsersAPIMessage.USER_ID_LABEL;

import java.io.IOException;
import java.util.Optional;

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

import com.vmware.athena.Athena;

import configurations.AthenaProperties;
import connections.AthenaConnectionPool;
import services.profiles.Consortium;
import services.profiles.Organization;
import services.profiles.ProfilesRegistryManager;
import services.profiles.Roles;
import services.profiles.UserCreateRequest;
import services.profiles.UserModificationException;
import services.profiles.UserPatchRequest;
import services.profiles.UsersAPIMessage;

/**
 * A servlet which manages all GET/POST/PATCH requests related to user management API of helen
 */
@Controller
public class ProfileManager extends BaseServlet {

    private static final long serialVersionUID = 1L;
    private static final Logger logger = LogManager.getLogger(ProfileManager.class);

    private ProfilesRegistryManager prm;

    @Autowired
    public ProfileManager(ProfilesRegistryManager prm, AthenaProperties config,
            AthenaConnectionPool athenaConnectionPool) {
        super(config, athenaConnectionPool);
        this.prm = prm;
    }

    // /api/users?consortium=<c>&organization=<o>
    @RequestMapping(path = "/api/users", method = RequestMethod.GET)
    public ResponseEntity<JSONAware> getUsers(@RequestParam(name = "consortium", required = false) String consortium,
            @RequestParam(name = "organization", required = false) String organization) {
        JSONArray result = prm.getUsers(Optional.ofNullable(consortium), Optional.ofNullable(organization));
        return new ResponseEntity<>(result, standardHeaders, HttpStatus.OK);
    }

    @RequestMapping(path = "/api/users/{user_id}", method = RequestMethod.GET)
    public ResponseEntity<JSONAware> getUserFromID(@PathVariable("user_id") String userID) {
        JSONObject result = prm.getUserWithID(userID);
        if (result.isEmpty()) {
            return new ResponseEntity<>(new JSONObject(), standardHeaders, HttpStatus.NOT_FOUND);
        } else {
            return new ResponseEntity<>(result, standardHeaders, HttpStatus.OK);
        }
    }

    private void createTestProfiles(JSONObject requestJson) {
        if (!requestJson.containsKey(ORGANIZATION_LABEL)) {
            Organization org = prm.createOrgIfNotExist();
            JSONObject orgJson = new JSONObject();
            orgJson.put(ORGANIZATION_ID_LABEL, org.getOrganizationID());
            requestJson.put(ORGANIZATION_LABEL, orgJson);
            logger.debug("New test org created with ID:" + org.getOrganizationID());
        }

        if (!requestJson.containsKey(CONSORTIUM_LABEL)) {
            Consortium consortium = prm.createConsortiumIfNotExist();
            JSONObject consJson = new JSONObject();
            consJson.put(CONSORTIUM_ID_LABEL, consortium.getConsortiumID());
            requestJson.put(CONSORTIUM_LABEL, consJson);
            logger.debug("New test consortium created with ID:" + consortium.getConsortiumID());
        }
    }

    private void validateCreateRequest(UserCreateRequest ucr) throws UserModificationException {
        if (ucr.getEmail() == null || ucr.getEmail().isEmpty()) {
            throw new UserModificationException("invalid email specified");
        }
        if (ucr.getUserName() == null || ucr.getEmail().isEmpty()) {
            throw new UserModificationException("invalid name specified");
        }
        if (ucr.getPassword() == null || ucr.getPassword().isEmpty()) {
            throw new UserModificationException("invalid password specified");
        }
    }

    @RequestMapping(path = "/api/users", method = RequestMethod.POST)
    public ResponseEntity<JSONAware> doPost(@RequestBody String requestBody) throws IOException {
        JSONObject responseJSON;
        HttpStatus responseStatus;
        try {
            JSONParser parser = new JSONParser();
            JSONObject requestJson = (JSONObject) parser.parse(requestBody);

            // TODO: Ideally the organization and consortium should already exist
            // before adding a USER to that. But for now we just add a new
            // organization & consortium to allow easy testing. Delete this
            // method once testing phase is done
            createTestProfiles(requestJson);
            UsersAPIMessage postRequest = new UsersAPIMessage(requestJson);

            validateCreateRequest(postRequest);

            String userID = prm.createUser(postRequest);

            responseJSON = new JSONObject();
            responseJSON.put(USER_ID_LABEL, userID);
            responseStatus = HttpStatus.OK;

        } catch (ParseException | UserModificationException e) {
            logger.warn("Error while adding new user", e);
            responseJSON = APIHelper.errorJSON(e.getMessage());
            responseStatus = HttpStatus.BAD_REQUEST;
        }

        return new ResponseEntity<>(responseJSON, standardHeaders, responseStatus);
    }

    private void validatePatchRequest(UserPatchRequest upr) throws UserModificationException {
        if (upr.getOptionalRole().isPresent() && !Roles.contains(upr.getOptionalRole().get())) {
            throw new UserModificationException("invalid role provided");
        }
        if (upr.getOptionalLastName().isPresent() && upr.getOptionalLastName().get().isEmpty()) {
            throw new UserModificationException("invalid last name provided");
        }
        if (upr.getOptionalFirstName().isPresent() && upr.getOptionalFirstName().get().isEmpty()) {
            throw new UserModificationException("invalid first name provided");
        }
        if (upr.getOptionalEmail().isPresent() && upr.getOptionalEmail().get().isEmpty()) {
            throw new UserModificationException("invalid email provided");
        }
        if (upr.getOptionalName().isPresent() && upr.getOptionalName().get().isEmpty()) {
            throw new UserModificationException("invalid name provided");
        }
    }

    @RequestMapping(path = "/api/users/{user_id}", method = RequestMethod.PATCH)
    public ResponseEntity<JSONAware> doPatch(@PathVariable(name = "user_id") String userID,
            @RequestBody String requestBody) {
        HttpStatus responseStatus;
        JSONObject responseJson;
        try {

            JSONParser parser = new JSONParser();
            JSONObject requestJson = (JSONObject) parser.parse(requestBody);
            UserPatchRequest upr = new UsersAPIMessage(requestJson);
            upr.setUserID(Long.parseLong(userID));
            validatePatchRequest(upr);
            prm.updateUser(upr);

            responseJson = new JSONObject();
            responseStatus = HttpStatus.OK;

        } catch (ParseException | UserModificationException e) {
            responseJson = APIHelper.errorJSON(e.getMessage());
            responseStatus = HttpStatus.BAD_REQUEST;
        }

        return new ResponseEntity<>(responseJson, standardHeaders, responseStatus);
    }

    @Override
    protected JSONAware parseToJSON(Athena.AthenaResponse athenaResponse) {
        throw new UnsupportedOperationException("parseToJSON method is not " + "supported in ProfileManager class");
    }
}

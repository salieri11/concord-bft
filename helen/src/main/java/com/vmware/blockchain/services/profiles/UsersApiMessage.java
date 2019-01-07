/*
 * Copyright (c) 2018-2019 VMware, Inc. All rights reserved. VMware Confidential
 */

package com.vmware.blockchain.services.profiles;

import java.util.Optional;
import java.util.UUID;

import org.json.simple.JSONObject;

/**
 * A class which holds all possible fields in a GET/POST/PATCH requests and responses sent to or returned by our system.
 * Do not use this object directly, always hold object in a reference of appropriate interface. This is because all
 * fields might not be populated in every request/response.
 */
public class UsersApiMessage implements UserCreateRequest, UserPatchRequest, UsersGetResponse {

    /*
     * TODO: These should be made protected once we add new API for organization and consortium creation
     */
    public static final String USER_ID_LABEL = "user_id";
    public static final String NAME_LABEL = "name";
    public static final String FIRST_NAME_LABEL = "first_name";
    public static final String LAST_NAME_LABEL = "last_name";
    public static final String EMAIL_LABEL = "email";
    public static final String PASSWORD_LABEL = "password";
    public static final String ROLE_LABEL = "role";
    public static final String LAST_LOGIN_LABEL = "last_login";
    public static final String DETAILS_LABEL = "details";

    public static final String ORGANIZATION_ID_LABEL = "organization_id";
    public static final String ORGANIZATION_NAME_LABEL = "organization_name";
    public static final String ORGANIZATION_LABEL = "organization";

    public static final String CONSORTIUM_LABEL = "consortium";
    public static final String CONSORTIUM_ID_LABEL = "consortium_id";
    public static final String CONSORTIUM_NAME_LABEL = "consortium_name";

    private UUID userId;
    private String userName;
    private String firstName;
    private String lastName;
    private String email;
    private String role;
    private String password;
    private Long lastLogin;
    private String organizationName;
    private String consortiumName;
    private UUID organizationId;
    private UUID consortiumId;

    private boolean isEmpty;

    public UsersApiMessage(JSONObject requestJson) {
        parseMessageJson(requestJson);
    }

    public UsersApiMessage(User user) {
        extractUserFields(user);
    }

    public UsersApiMessage() {
        isEmpty = true;
    }

    private void extractUserFields(User user) {
        this.userId = user.getUserId();
        this.userName = user.getName();
        this.firstName = user.getFirstName();
        this.lastName = user.getLastName();
        this.email = user.getEmail();
        this.role = user.getRole();
        this.lastLogin = user.getLastLogin();
        this.organizationId = user.getOrganization().getOrganizationId();
        this.organizationName = user.getOrganization().getOrganizationName();
        this.consortiumId = user.getConsortium().getConsortiumId();
        this.consortiumName = user.getConsortium().getConsortiumName();
    }

    private void extractUserFields(JSONObject request) {
        if (request.containsKey(NAME_LABEL)) {
            userName = (String) request.get(NAME_LABEL);
        }
        if (request.containsKey(EMAIL_LABEL)) {
            email = (String) request.get(EMAIL_LABEL);
        }
        if (request.containsKey(PASSWORD_LABEL)) {
            password = (String) request.get(PASSWORD_LABEL);
        }
        if (request.containsKey(ROLE_LABEL)) {
            role = (String) request.get(ROLE_LABEL);
        }
        if (request.containsKey(DETAILS_LABEL)) {
            JSONObject details = (JSONObject) request.get(DETAILS_LABEL);
            if (details.containsKey(FIRST_NAME_LABEL)) {
                firstName = (String) details.get(FIRST_NAME_LABEL);
            }
            if (details.containsKey(LAST_NAME_LABEL)) {
                lastName = (String) details.get(LAST_NAME_LABEL);
            }
        }
    }

    private void extractOrganizationFields(JSONObject request) {
        if (request.containsKey(ORGANIZATION_LABEL)) {
            JSONObject orgJson = (JSONObject) request.get(ORGANIZATION_LABEL);
            if (orgJson.containsKey(ORGANIZATION_ID_LABEL)) {
                organizationId = (UUID) orgJson.get(ORGANIZATION_ID_LABEL);
            }
            if (orgJson.containsKey(ORGANIZATION_NAME_LABEL)) {
                organizationName = (String) orgJson.get(ORGANIZATION_NAME_LABEL);
            }
        }
    }

    private void extractConsortiumFields(JSONObject request) {
        if (request.containsKey(CONSORTIUM_LABEL)) {
            JSONObject conJson = (JSONObject) request.get(CONSORTIUM_LABEL);
            if (conJson.containsKey(CONSORTIUM_ID_LABEL)) {
                consortiumId = (UUID) conJson.get(CONSORTIUM_ID_LABEL);
            }
            if (conJson.containsKey(CONSORTIUM_NAME_LABEL)) {
                consortiumName = (String) conJson.get(CONSORTIUM_NAME_LABEL);
            }
        }
    }

    private void parseMessageJson(JSONObject request) {
        extractUserFields(request);
        extractOrganizationFields(request);
        extractConsortiumFields(request);
    }

    public UUID getUserId() {
        return userId;
    }

    /**
     * set method is only added for userID because it is part of URI and not the JSON object. All other attributes are
     * part of JSON and will be set by reading the request JSON
     *
     * @param userId user id
     */
    public void setUserId(UUID userId) {
        this.userId = userId;
    }

    @Override
    public Optional<String> getOptionalRole() {
        return Optional.ofNullable(role);
    }

    @Override
    public Optional<String> getOptionalFirstName() {
        return Optional.ofNullable(firstName);
    }

    @Override
    public Optional<String> getOptionalLastName() {
        return Optional.ofNullable(lastName);
    }

    @Override
    public Optional<String> getOptionalEmail() {
        return Optional.ofNullable(email);
    }

    @Override
    public Optional<String> getOptionalName() {
        return Optional.ofNullable(userName);
    }

    public String getUserName() {
        return userName;
    }

    public String getFirstName() {
        return firstName;
    }

    public String getLastName() {
        return lastName;
    }

    public String getEmail() {
        return email;
    }

    public String getRole() {
        return role;
    }

    public String getPassword() {
        return password;
    }

    public Long getLastLogin() {
        return lastLogin;
    }

    public String getOrganizationName() {
        return organizationName;
    }

    public String getConsortiumName() {
        return consortiumName;
    }

    public UUID getOrganizationId() {
        return organizationId;
    }

    public UUID getConsortiumId() {
        return consortiumId;
    }

    @Override
    public JSONObject toJson() {
        JSONObject json = new JSONObject();
        if (isEmpty) {
            return json;
        }

        JSONObject details = new JSONObject();
        details.put(FIRST_NAME_LABEL, firstName);
        details.put(LAST_NAME_LABEL, lastName);

        JSONObject orgJson = new JSONObject();
        orgJson.put(ORGANIZATION_ID_LABEL, organizationId);
        orgJson.put(ORGANIZATION_NAME_LABEL, organizationName);

        JSONObject conJson = new JSONObject();
        conJson.put(CONSORTIUM_ID_LABEL, consortiumId);
        conJson.put(CONSORTIUM_NAME_LABEL, consortiumName);

        json.put(USER_ID_LABEL, userId);
        json.put(NAME_LABEL, userName);
        json.put(DETAILS_LABEL, details);
        json.put(EMAIL_LABEL, email);
        json.put(ROLE_LABEL, role);
        json.put(LAST_LOGIN_LABEL, lastLogin);
        json.put(ORGANIZATION_LABEL, orgJson);
        json.put(CONSORTIUM_LABEL, conJson);

        return json;
    }
}

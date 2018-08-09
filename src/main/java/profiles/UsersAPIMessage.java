/**
 * <p>
 * Copyright 2018 VMware, all rights reserved.
 * </p>
 *
 */

package profiles;

import java.time.Instant;
import java.util.Optional;

import org.json.simple.JSONObject;

/**
 * A class which holds all possible fields in a GET/POST/PATCH requests and
 * responses sent to or returned by our system. Do not use this object directly,
 * always hold object in a reference of appropriate interface. This is because
 * all fields might not be populated in every request/response.
 */
public class UsersAPIMessage implements UserCreateRequest, UserPatchRequest,
                             UsersGetResponse {

   /*
    * TODO: These should be made protected once we add new API for organization
    * and consortium creation
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

   private Long userID;
   private String userName;
   private String firstName;
   private String lastName;
   private String email;
   private String role;
   private String password;
   private Long lastLogin;
   private String organizationName;
   private String consortiumName;
   private Long organizationID;
   private Long consortiumID;

   private boolean isEmpty;

   public UsersAPIMessage(JSONObject requestJSON) {
      parseMessageJSON(requestJSON);
   }

   public UsersAPIMessage(User user) {
      extractUserFields(user);
   }

   public UsersAPIMessage() {
      isEmpty = true;
   }

   private void extractUserFields(User user) {
      this.userID = user.getUserID();
      this.userName = user.getName();
      this.firstName = user.getFirstName();
      this.lastName = user.getLastName();
      this.email = user.getEmail();
      this.role = user.getRole();
      this.lastLogin = user.getLastLogin();
      this.organizationID = user.getOrganization().getOrganizationID();
      this.organizationName = user.getOrganization().getOrganizationName();
      this.consortiumID = user.getConsortium().getConsortiumID();
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
         JSONObject orgJSON = (JSONObject) request.get(ORGANIZATION_LABEL);
         if (orgJSON.containsKey(ORGANIZATION_ID_LABEL)) {
            organizationID = (Long) orgJSON.get(ORGANIZATION_ID_LABEL);
         }
         if (orgJSON.containsKey(ORGANIZATION_NAME_LABEL)) {
            organizationName = (String) orgJSON.get(ORGANIZATION_NAME_LABEL);
         }
      }
   }

   private void extractConsortiumFields(JSONObject request) {
      if (request.containsKey(CONSORTIUM_LABEL)) {
         JSONObject conJSON = (JSONObject) request.get(CONSORTIUM_LABEL);
         if (conJSON.containsKey(CONSORTIUM_ID_LABEL)) {
            consortiumID = (Long) conJSON.get(CONSORTIUM_ID_LABEL);
         }
         if (conJSON.containsKey(CONSORTIUM_NAME_LABEL)) {
            consortiumName = (String) conJSON.get(CONSORTIUM_NAME_LABEL);
         }
      }
   }

   private void parseMessageJSON(JSONObject request) {
      extractUserFields(request);
      extractOrganizationFields(request);
      extractConsortiumFields(request);
   }

   public Long getUserID() {
      return userID;
   }

   /**
    * set method is only added for userID because it is part of URI and not the
    * JSON object. All other attributes are part of JSON and will be set by
    * reading the request JSON
    * 
    * @param userID
    */
   public void setUserID(Long userID) {
      this.userID = userID;
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

   public Long getOrganizationID() {
      return organizationID;
   }

   public Long getConsortiumID() {
      return consortiumID;
   }

   @Override
   public JSONObject toJSON() {
      JSONObject json = new JSONObject();
      if (isEmpty)
         return json;

      JSONObject details = new JSONObject();
      details.put(FIRST_NAME_LABEL, firstName);
      details.put(LAST_NAME_LABEL, lastName);

      JSONObject orgJson = new JSONObject();
      orgJson.put(ORGANIZATION_ID_LABEL, organizationID);
      orgJson.put(ORGANIZATION_NAME_LABEL, organizationName);

      JSONObject conJson = new JSONObject();
      conJson.put(CONSORTIUM_ID_LABEL, consortiumID);
      conJson.put(CONSORTIUM_NAME_LABEL, consortiumName);

      json.put(USER_ID_LABEL, userID);
      json.put(NAME_LABEL, userName);
      json.put(DETAILS_LABEL, details);
      json.put(EMAIL_LABEL, email);
      json.put(ROLE_LABEL, role);
      json.put(LAST_LOGIN_LABEL, lastLogin.toString());
      json.put(ORGANIZATION_LABEL, orgJson);
      json.put(CONSORTIUM_LABEL, conJson);

      return json;
   }
}

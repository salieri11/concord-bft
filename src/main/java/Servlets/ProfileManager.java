/**
 * <p>
 * Copyright 2018 VMware, all rights reserved.
 * </p>
 *
 */

package Servlets;

import static profiles.UsersAPIMessage.*;

import java.io.IOException;
import java.util.Optional;
import java.util.stream.Collectors;

import javax.servlet.ServletException;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import org.apache.log4j.Logger;
import org.json.simple.JSONArray;
import org.json.simple.JSONAware;
import org.json.simple.JSONObject;
import org.json.simple.parser.JSONParser;
import org.json.simple.parser.ParseException;

import com.vmware.athena.Athena;

import profiles.*;

/**
 * A servlet which manages all GET/POST/PATCH requests related to user
 * management API of helen
 */
public class ProfileManager extends BaseServlet {

   private static final long serialVersionUID = 1L;
   private static final Logger logger = Logger.getLogger(ProfileManager.class);

   private ProfilesRegistryManager prm = null;

   public ProfileManager() {
   }

   @Override
   public void init() {
      prm = BeanUtil.getBean(ProfilesRegistryManager.class);
   }

   @Override
   protected void service(HttpServletRequest request,
                          HttpServletResponse response) throws ServletException,
                                                        IOException {

      if (request.getMethod().equalsIgnoreCase("PATCH")) {
         doPatch(request, response);
      } else {
         super.service(request, response);
      }
   }

   @Override
   protected void doGet(HttpServletRequest request,
                        HttpServletResponse response) throws IOException {
      String uri = request.getRequestURI();
      if (uri.endsWith("/"))
         uri = uri.substring(0, uri.length() - 1);

      String uriTokens[] = uri.split("/");
      String responseString;
      int responseStatus;
      String consortium = request.getParameter("consortium");
      String organization = request.getParameter("organization");

      if (uriTokens.length == 3) {
         // /api/users?consortium=<c>&organization=<o>
         JSONArray result = prm.getUsers(Optional.ofNullable(consortium),
                 Optional.ofNullable(organization));
         responseString = result.toJSONString();
         responseStatus = HttpServletResponse.SC_OK;
      } else if (uriTokens.length == 4) {
         // /api/users/<userid>
         JSONObject result = prm.getUserWithID(uriTokens[3]);
         responseString = result.toJSONString();
         if (result.isEmpty()) {
            responseStatus = HttpServletResponse.SC_NOT_FOUND;
         } else {
            responseStatus = HttpServletResponse.SC_OK;
         }
      } else {
         responseString = new JSONObject().toJSONString();
         responseStatus = HttpServletResponse.SC_NOT_FOUND;
      }
      processResponse(response, responseString, responseStatus, logger);
   }

   private void createTestProfiles(JSONObject requestJson) {
      if (!requestJson.containsKey(ORGANIZATION_LABEL)) {
         Long organizationId = prm.createOrgIfNotExist();
         JSONObject orgJson = new JSONObject();
         orgJson.put(ORGANIZATION_ID_LABEL, organizationId);
         requestJson.put(ORGANIZATION_LABEL, orgJson);
         logger.debug("New test org created with ID:" + organizationId);
      }

      if (!requestJson.containsKey(CONSORTIUM_LABEL)) {
         Long consortiumId = prm.createConsortiumIfNotExist();
         JSONObject consJson = new JSONObject();
         consJson.put(CONSORTIUM_ID_LABEL, consortiumId);
         requestJson.put(CONSORTIUM_LABEL, consJson);
         logger.debug("New test consortium created with ID:" + consortiumId);
      }
   }

   private void
           validateCreateRequest(UserCreateRequest ucr) throws UserModificationException {
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
   
   @Override
   protected void
             doPost(final HttpServletRequest request,
                    final HttpServletResponse response) throws IOException {
      JSONObject responseJSON;
      int responseStatus;
      try {

         JSONParser parser = new JSONParser();
         JSONObject requestJson
            = (JSONObject) parser.parse(APIHelper.getRequestBody(request));

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
         responseStatus = HttpServletResponse.SC_OK;

      } catch (ParseException | UserModificationException e) {
         logger.warn("Error while adding new user", e);
         responseJSON = APIHelper.errorJSON(e.getMessage());
         responseStatus = HttpServletResponse.SC_BAD_REQUEST;
      }

      processResponse(response,
                      responseJSON.toJSONString(),
                      responseStatus,
                      logger);
   }
   
   
   private void
   validatePatchRequest(UserPatchRequest upr) throws UserModificationException {
      if (upr.getOptionalRole().isPresent() &&
              !Roles.contains(upr.getOptionalRole().get())) {
         throw new UserModificationException("invalid role provided");
      }
      if (upr.getOptionalLastName().isPresent() &&
              upr.getOptionalLastName().get().isEmpty()) {
         throw new UserModificationException("invalid last name provided");
      }
      if (upr.getOptionalFirstName().isPresent() &&
              upr.getOptionalFirstName().get().isEmpty()) {
         throw new UserModificationException("invalid first name provided");
      }
      if (upr.getOptionalEmail().isPresent() &&
              upr.getOptionalEmail().get().isEmpty()) {
         throw new UserModificationException("invalid email provided");
      }
      if (upr.getOptionalName().isPresent() &&
              upr.getOptionalName().get().isEmpty()){
         throw new UserModificationException("invalid name provided");
      }
   }
   
   protected void doPatch(HttpServletRequest request,
                          HttpServletResponse response) {
      int responseStatus;
      String responseString;
      try {
         String paramString
            = request.getReader()
                     .lines()
                     .collect(Collectors.joining(System.lineSeparator()));
         // get userID from path parameter
         String uri = request.getRequestURI();
         if (uri.endsWith("/")) {
            uri = uri.substring(0, uri.length() - 1);
         }
         String uriTokens[] = uri.split("/");

         if (uriTokens.length == 4) {
            String userID = uriTokens[3];
            JSONParser parser = new JSONParser();
            JSONObject requestJson = (JSONObject) parser.parse(paramString);
            UserPatchRequest upr = new UsersAPIMessage(requestJson);
            upr.setUserID(Long.parseLong(userID));
            validatePatchRequest(upr);
            prm.updateUser(upr);

            responseString = new JSONObject().toJSONString();
            responseStatus = HttpServletResponse.SC_OK;
         } else {
            responseString = new JSONObject().toJSONString();
            responseStatus = HttpServletResponse.SC_NOT_FOUND;
         }

      } catch (IOException | ParseException | UserModificationException e) {
         responseString = APIHelper.errorJSON(e.getMessage()).toJSONString();
         responseStatus = HttpServletResponse.SC_BAD_REQUEST;
      }

      processResponse(response, responseString, responseStatus, logger);
   }

   @Override
   protected JSONAware parseToJSON(Athena.AthenaResponse athenaResponse) {
      throw new UnsupportedOperationException("parseToJSON method is not "
         + "supported in ProfileManager class");
   }
}

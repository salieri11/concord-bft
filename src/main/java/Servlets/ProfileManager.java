package Servlets;

import static profiles.Consortium.CONSORTIUM_ID_LABEL;
import static profiles.Consortium.CONSORTIUM_LABEL;
import static profiles.Organization.ORGANIZATION_ID_LABEL;
import static profiles.Organization.ORGANIZATION_LABEL;
import static profiles.User.*;
import static profiles.UserRole.ROLE_LABEL;

import java.io.IOException;
import java.sql.SQLException;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.regex.Pattern;
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

public class ProfileManager extends BaseServlet {

   private static final long serialVersionUID = 1L;
   private static final Logger logger = Logger.getLogger(ProfileManager.class);
   private UsersRegistryManager urm;

   private ProfileManager() {
      try {
         urm = UsersRegistryManager.getInstance();
      } catch (Exception e) {
         logger.fatal("Unable to instantiate UsersRegistryManager", e);
         urm = null;
      }
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

      if (consortium != null && organization != null && uriTokens.length == 3) {
         // /api/user?consortium=<c>&organization=<o>
         JSONArray result = urm.getUsers(consortium, organization)
                               .stream()
                               .map(UserRole::getUserID)
                               .map((id) -> urm.getUserWithID(id))
                               .flatMap(List::stream)
                               .map(User::toJSON)
                               .reduce(new JSONArray(), (arr, obj) -> {
                                  arr.add(obj);
                                  return arr;
                               }, (arr1, arr2) -> {
                                  arr1.addAll(arr2);
                                  return arr1;
                               });

         responseString = result.toJSONString();
         responseStatus = HttpServletResponse.SC_OK;

      } else if (uriTokens.length == 4) {
         // /api/user/<userid>
         JSONObject result = urm.getUserWithID(uriTokens[3])
                                .stream()
                                .map(User::toJSON)
                                .findFirst()
                                .orElse(new JSONObject());

         responseString = result.toJSONString();
         responseStatus = HttpServletResponse.SC_OK;
      } else {
         responseString = "";
         responseStatus = HttpServletResponse.SC_NOT_FOUND;
      }
      processResponse(response, responseString, responseStatus, logger);
   }

   private Map<String, String>
           parseRequestJSON(String jsonString) throws ParseException {
      JSONParser parser = new JSONParser();
      JSONObject requestObject = (JSONObject) parser.parse(jsonString);

      Map<String, String> jsonData = new HashMap<>();

      jsonData.put(NAME_LABEL, (String) requestObject.get(NAME_LABEL));
      jsonData.put(EMAIL_LABEL, (String) requestObject.get(EMAIL_LABEL));
      jsonData.put(ROLE_LABEL, (String) requestObject.get(ROLE_LABEL));
      jsonData.put(PASSWORD_LABEL, (String) requestObject.get(PASSWORD_LABEL));

      if (requestObject.containsKey(ORGANIZATION_LABEL)) {
         JSONObject organization
            = (JSONObject) requestObject.get(ORGANIZATION_LABEL);
         jsonData.put(ORGANIZATION_ID_LABEL,
                      (String) organization.get(ORGANIZATION_ID_LABEL));
      }

      if (requestObject.containsKey(CONSORTIUM_LABEL)) {
         JSONObject consortium
            = (JSONObject) requestObject.get(CONSORTIUM_LABEL);
         jsonData.put(CONSORTIUM_ID_LABEL,
                      (String) consortium.get(CONSORTIUM_ID_LABEL));
      }

      return jsonData;
   }

   private void createTestProfilesAndFillMap(Map<String, String> jsonData) {
      if (!jsonData.containsKey(ORGANIZATION_ID_LABEL)) {
         String organizationId = urm.createOrgIfNotExist("TEST_ORGANIZATION");
         jsonData.put(ORGANIZATION_ID_LABEL, organizationId);
         logger.debug("New test org created with ID:" + organizationId);
      }

      if (!jsonData.containsKey(CONSORTIUM_ID_LABEL)) {
         String consortiumId
            = urm.createConsortiumIfNotExist("TEST_CONSORTIUM", "DEFAULT");
         jsonData.put(CONSORTIUM_ID_LABEL, consortiumId);
         logger.debug("New test consortium created with ID:" + consortiumId);
      }
   }

   @Override
   protected void
             doPost(final HttpServletRequest request,
                    final HttpServletResponse response) throws IOException {
      String responseString;
      int responseStatus;
      try {
         String paramString
            = request.getReader()
                     .lines()
                     .collect(Collectors.joining(System.lineSeparator()));

         Map<String, String> jsonData = parseRequestJSON(paramString);

         // TODO: Ideally the organization and consortium should already exist
         // before adding a USER to that. But for now we just add a new
         // organization & consortium to allow easy testing. Delete this
         // method once testing phase is done
         createTestProfilesAndFillMap(jsonData);

         
         String userID = urm.createUser(jsonData.get(NAME_LABEL),
                                        jsonData.get(EMAIL_LABEL),
                                        jsonData.get(ROLE_LABEL),
                                        jsonData.get(CONSORTIUM_ID_LABEL),
                                        jsonData.get(ORGANIZATION_ID_LABEL),
                                        jsonData.get(PASSWORD_LABEL));

         JSONObject responseJSON = new JSONObject();
         responseJSON.put(USER_ID_LABEL, userID);
         responseStatus = HttpServletResponse.SC_OK;
         responseString = responseJSON.toJSONString();

      } catch (ParseException pe) {
         logger.warn("Error while parsing request JSON", pe);
         responseString = APIHelper.errorJSON("Invalid JSON").toJSONString();
         responseStatus = HttpServletResponse.SC_BAD_REQUEST;
      } catch (UserModificationException ue) {
         logger.warn("Error while adding new user", ue);
         responseString = APIHelper.errorJSON(ue.getMessage()).toJSONString();
         // TODO: maybe we should throw different types of exceptions for
         // different types of error and set status code accordingly
         responseStatus = HttpServletResponse.SC_EXPECTATION_FAILED;
      }
      processResponse(response, responseString, responseStatus, logger);
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
         if (uri.endsWith("/"))
            uri = uri.substring(0, uri.length() - 1);
         String uriTokens[] = uri.split("/");
         if (uriTokens.length == 4) {
            String userID = uriTokens[3];
            Map<String, String> jsonData = parseRequestJSON(paramString);
            UserPatchRequest upr = new UserPatchRequest(userID, jsonData);
            if (urm.updateUser(upr)) {
               responseString = "";
               responseStatus = HttpServletResponse.SC_OK;
            } else {
               responseString = "Update failed";
               responseStatus = HttpServletResponse.SC_INTERNAL_SERVER_ERROR;
            }
         } else {
            responseString = "";
            responseStatus = HttpServletResponse.SC_NOT_FOUND;
         }
         
      } catch (IOException e) {
         responseString = APIHelper.errorJSON(e.getMessage()).toJSONString();
         responseStatus = HttpServletResponse.SC_BAD_REQUEST;
      } catch (ParseException | SQLException e) {
         responseString = APIHelper.errorJSON(e.getMessage()).toJSONString();
         responseStatus = HttpServletResponse.SC_EXPECTATION_FAILED;
      }
      
      processResponse(response, responseString, responseStatus, logger);
   }

   @Override
   protected JSONAware parseToJSON(Athena.AthenaResponse athenaResponse) {
      throw new UnsupportedOperationException("parseToJSON method is not " +
              "supported in ProfileManager class");
   }
}

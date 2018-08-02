package Servlets;

import static profiles.Consortium.CONSORTIUM_ID_LABEL;
import static profiles.Consortium.CONSORTIUM_LABEL;
import static profiles.Organization.ORGANIZATION_ID_LABEL;
import static profiles.Organization.ORGANIZATION_LABEL;
import static profiles.User.*;

import java.io.IOException;
import java.util.HashMap;
import java.util.Map;
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

import profiles.UserModificationException;
import profiles.UserPatchRequest;
import profiles.UsersRegistryManager;

public class ProfileManager extends BaseServlet {

   private static final long serialVersionUID = 1L;
   private static final Logger logger = Logger.getLogger(ProfileManager.class);

   private UsersRegistryManager urm = null;

   public ProfileManager() {
   }

   @Override
   public void init() {
      urm = BeanUtil.getBean(UsersRegistryManager.class);
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
         JSONArray result = urm.getUsers(consortium, organization);
         responseString = result.toJSONString();
         responseStatus = HttpServletResponse.SC_OK;

      } else if (uriTokens.length == 4) {
         // /api/user/<userid>
         JSONObject result = urm.getUserWithID(uriTokens[3]);
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

   private Map<String, String>
           parseRequestJSON(String jsonString) throws ParseException {
      JSONParser parser = new JSONParser();
      JSONObject requestObject = (JSONObject) parser.parse(jsonString);

      Map<String, String> jsonData = new HashMap<>();

      String labels[] = new String[] { NAME_LABEL, EMAIL_LABEL, ROLE_LABEL,
         PASSWORD_LABEL, ORGANIZATION_LABEL, CONSORTIUM_LABEL, DETAILS_LABEL};

      for (String label : labels) {
         if (requestObject.containsKey(label)) {
            if (label.equals(ORGANIZATION_LABEL)) {
               JSONObject organization
                  = (JSONObject) requestObject.get(ORGANIZATION_LABEL);
               jsonData.put(ORGANIZATION_ID_LABEL,
                            (String) organization.get(ORGANIZATION_ID_LABEL));
            } else if (label.equals(CONSORTIUM_LABEL)) {
               JSONObject consortium
                  = (JSONObject) requestObject.get(CONSORTIUM_LABEL);
               jsonData.put(CONSORTIUM_ID_LABEL,
                            (String) consortium.get(CONSORTIUM_ID_LABEL));
            } else if (label.equals(DETAILS_LABEL)) {
               JSONObject details =
                       (JSONObject) requestObject.get(DETAILS_LABEL);
               if (details.containsKey(FIRST_NAME_LABEL)) {
                  jsonData.put(FIRST_NAME_LABEL,
                          (String) details.get(FIRST_NAME_LABEL));
               }
               if (details.containsKey(LAST_NAME_LABEL)) {
                  jsonData.put(LAST_NAME_LABEL,
                          (String) details.get(LAST_NAME_LABEL));
               }
            } else {
               jsonData.put(label, (String) requestObject.get(label));
            }
         }
      }
      return jsonData;
   }

   private void createTestProfilesAndFillMap(Map<String, String> jsonData) {
      if (!jsonData.containsKey(ORGANIZATION_ID_LABEL)) {
         String organizationId = Long.toString(urm.createOrgIfNotExist());
         jsonData.put(ORGANIZATION_ID_LABEL, organizationId);
         logger.debug("New test org created with ID:" + organizationId);
      }

      if (!jsonData.containsKey(CONSORTIUM_ID_LABEL)) {
         String consortiumId = Long.toString(urm.createConsortiumIfNotExist());
         jsonData.put(CONSORTIUM_ID_LABEL, consortiumId);
         logger.debug("New test consortium created with ID:" + consortiumId);
      }
   }

   private String
           getRequestBody(final HttpServletRequest request) throws IOException {
      String paramString
         = request.getReader()
                  .lines()
                  .collect(Collectors.joining(System.lineSeparator()));
      return paramString;
   }

   protected RESTResult
             login(final HttpServletRequest request,
                   final HttpServletResponse response) throws IOException {
      JSONParser parser = new JSONParser();
      RESTResult result;
      try {
         JSONObject requestJSON
            = (JSONObject) parser.parse(getRequestBody(request));
         String uri = request.getRequestURI();
         if (uri.endsWith("/")) {
            uri = uri.substring(0, uri.length() - 1);
         }
         String tokens[] = uri.split("/");
         // URI is /api/user/login/<userID>
         if (tokens[3].equals("login")) {
            boolean successful
               = urm.loginUser(tokens[4],
                               (String) requestJSON.get(PASSWORD_LABEL));
            if (successful) {
               result
                  = new RESTResult(HttpServletResponse.SC_OK, new JSONObject());
            } else {
               result = new RESTResult(HttpServletResponse.SC_FORBIDDEN,
                                       new JSONObject());
            }
         } else {
            result = new RESTResult(HttpServletResponse.SC_NOT_FOUND,
                                    new JSONObject());
         }
      } catch (ParseException e) {
         result = new RESTResult(HttpServletResponse.SC_BAD_REQUEST,
                                 APIHelper.errorJSON("Invalid JSON"));
      } catch (UserModificationException e) {
         result = new RESTResult(HttpServletResponse.SC_EXPECTATION_FAILED,
                                 APIHelper.errorJSON(e.getMessage()));
      }
      return result;
   }

   protected RESTResult
             handlePost(final HttpServletRequest request,
                        final HttpServletResponse response) throws IOException {
      JSONObject responseJSON;
      int responseStatus;
      try {

         Map<String, String> jsonData
            = parseRequestJSON(getRequestBody(request));

         // TODO: Ideally the organization and consortium should already exist
         // before adding a USER to that. But for now we just add a new
         // organization & consortium to allow easy testing. Delete this
         // method once testing phase is done
         createTestProfilesAndFillMap(jsonData);

         String userID
            = urm.createUser(jsonData.get(NAME_LABEL),
                             jsonData.get(EMAIL_LABEL),
                             jsonData.get(ROLE_LABEL),
                             Optional.ofNullable(jsonData.get(FIRST_NAME_LABEL)),
                             Optional.ofNullable(jsonData.get(LAST_NAME_LABEL)),
                             jsonData.get(CONSORTIUM_ID_LABEL),
                             jsonData.get(ORGANIZATION_ID_LABEL),
                             jsonData.get(PASSWORD_LABEL));

         responseJSON = new JSONObject();
         responseJSON.put(USER_ID_LABEL, userID);
         responseStatus = HttpServletResponse.SC_OK;

      } catch (ParseException pe) {
         logger.warn("Error while parsing request JSON", pe);
         responseJSON = APIHelper.errorJSON("Invalid JSON");
         responseStatus = HttpServletResponse.SC_BAD_REQUEST;
      } catch (UserModificationException ue) {
         logger.warn("Error while adding new user", ue);
         responseJSON = APIHelper.errorJSON(ue.getMessage());
         // TODO: maybe we should throw different types of exceptions for
         // different types of error and set status code accordingly
         responseStatus = HttpServletResponse.SC_EXPECTATION_FAILED;
      }

      return new RESTResult(responseStatus, responseJSON);
   }

   @Override
   protected void
             doPost(final HttpServletRequest request,
                    final HttpServletResponse response) throws IOException {
      String uri = request.getRequestURI();
      RESTResult restResult;
      if (uri.contains("login")) {
         restResult = login(request, response);
      } else {
         restResult = handlePost(request, response);
      }
      processResponse(response,
                      restResult.json.toJSONString(),
                      restResult.responseStatus,
                      logger);
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
            Map<String, String> jsonData = parseRequestJSON(paramString);
            urm.updateUser(new UserPatchRequest(userID, jsonData));
            responseString = new JSONObject().toJSONString();
            responseStatus = HttpServletResponse.SC_OK;
         } else {
            responseString = new JSONObject().toJSONString();
            responseStatus = HttpServletResponse.SC_NOT_FOUND;
         }

      } catch (IOException e) {
         responseString = APIHelper.errorJSON(e.getMessage()).toJSONString();
         responseStatus = HttpServletResponse.SC_BAD_REQUEST;
      } catch (ParseException e) {
         responseString = APIHelper.errorJSON(e.getMessage()).toJSONString();
         responseStatus = HttpServletResponse.SC_EXPECTATION_FAILED;
      } catch (UserModificationException e) {
         responseString = APIHelper.errorJSON(e.getMessage()).toJSONString();
         responseStatus = HttpServletResponse.SC_INTERNAL_SERVER_ERROR;
      }

      processResponse(response, responseString, responseStatus, logger);
   }

   @Override
   protected JSONAware parseToJSON(Athena.AthenaResponse athenaResponse) {
      throw new UnsupportedOperationException("parseToJSON method is not "
         + "supported in ProfileManager class");
   }

   private static class RESTResult {
      private int responseStatus; // Http status code of response
      // The response can either be a jsonObject or jsonArray.
      private JSONAware json;

      public RESTResult(int responseStatus, JSONAware json) {
         this.responseStatus = responseStatus;
         this.json = json;
      }

      /**
       * Returns the string object of jsonObject or jsonArray (whichever is
       * present).
       *
       * @return json string of result
       */
      public String getResultString() {
         return json.toJSONString();
      }

      public String toString() {
         return "Status: " + responseStatus + " Result: " + json.toJSONString();
      }
   }
}

package Servlets;

import java.io.IOException;
import java.util.Map;
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

import profiles.ProfilesRegistryManager;
import profiles.UserModificationException;
import profiles.UserPatchRequest;
import profiles.UsersAPIMessage;

import static profiles.UsersAPIMessage.*;

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

      if (consortium != null && organization != null && uriTokens.length == 3) {
         // /api/user?consortium=<c>&organization=<o>
         JSONArray result = prm.getUsers(consortium, organization);
         responseString = result.toJSONString();
         responseStatus = HttpServletResponse.SC_OK;

      } else if (uriTokens.length == 4) {
         // /api/user/<userid>
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

   private String
           getRequestBody(final HttpServletRequest request) throws IOException {
      String paramString
         = request.getReader()
                  .lines()
                  .collect(Collectors.joining(System.lineSeparator()));
      return paramString;
   }

   // TODO: This is not a proper way to authenticate the user. We have plans to
   // authenticate every user via CSP, however that integration will take time
   // and till then some way of authentication is needed. Hence, we have added
   // this temporary (and not very secure) login feature. Remove this and
   // authenticate every user with CSP
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
               = prm.loginUser(tokens[4],
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
                                 APIHelper.errorJSON(e.getMessage()));
      } catch (UserModificationException e) {
         result = new RESTResult(HttpServletResponse.SC_EXPECTATION_FAILED,
                                 APIHelper.errorJSON(e.getMessage()));
      }
      return result;
   }
   
   
   private void createTestProfiles(JSONObject requestJson) {
      if (!requestJson.containsKey(ORGANIZATION_LABEL)) {
         String organizationId = Long.toString(prm.createOrgIfNotExist());
         JSONObject orgJson = new JSONObject();
         orgJson.put(ORGANIZATION_ID_LABEL, organizationId);
         requestJson.put(ORGANIZATION_LABEL, orgJson);
         logger.debug("New test org created with ID:" + organizationId);
      }
      
      if (!requestJson.containsKey(CONSORTIUM_LABEL)) {
         String consortiumId = Long.toString(prm.createConsortiumIfNotExist());
         JSONObject consJson = new JSONObject();
         consJson.put(CONSORTIUM_ID_LABEL, consortiumId);
         requestJson.put(CONSORTIUM_LABEL, consJson);
         logger.debug("New test consortium created with ID:" + consortiumId);
      }
   }
   
   protected RESTResult
             handlePost(final HttpServletRequest request,
                        final HttpServletResponse response) throws IOException {
      JSONObject responseJSON;
      int responseStatus;
      try {
   
         JSONParser parser = new JSONParser();
         JSONObject requestJson =
                 (JSONObject) parser.parse(getRequestBody(request));
   
         // TODO: Ideally the organization and consortium should already exist
         // before adding a USER to that. But for now we just add a new
         // organization & consortium to allow easy testing. Delete this
         // method once testing phase is done
         createTestProfiles(requestJson);
         
         UsersAPIMessage postRequest =
                 new UsersAPIMessage(requestJson);


         String userID
            = prm.createUser(postRequest);

         responseJSON = new JSONObject();
         responseJSON.put(USER_ID_LABEL, userID);
         responseStatus = HttpServletResponse.SC_OK;

      } catch (ParseException | UserModificationException e) {
         logger.warn("Error while adding new user", e);
         responseJSON = APIHelper.errorJSON(e.getMessage());
         responseStatus = HttpServletResponse.SC_BAD_REQUEST;
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
            JSONParser parser = new JSONParser();
            JSONObject requestJson = (JSONObject) parser.parse(paramString);
            UserPatchRequest upr = new UsersAPIMessage(requestJson);
            upr.setUserID(Long.parseLong(userID));
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

/**
 * <p>
 * Copyright 2018 VMware, all rights reserved.
 * </p>
 *
 */

package Servlets;

import static profiles.UsersAPIMessage.PASSWORD_LABEL;

import java.io.IOException;

import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import org.apache.log4j.Logger;
import org.json.simple.JSONAware;
import org.json.simple.JSONObject;
import org.json.simple.parser.JSONParser;
import org.json.simple.parser.ParseException;

import com.vmware.athena.Athena;

import profiles.ProfilesRegistryManager;
import profiles.UserModificationException;

/**
 * A servlet for handling the user authentication flow of helen. This servlet is
 * just added for temporary usage. Actual authentication will be done with CSP.
 * Do NOT rely on this servlet for primary authentication method.
 */
public class UserAuthenticator extends BaseServlet {

   private static final Logger logger
      = Logger.getLogger(UserAuthenticator.class);

   private ProfilesRegistryManager prm = null;

   @Override
   public void init() {
      prm = BeanUtil.getBean(ProfilesRegistryManager.class);
   }

   @Override
   protected void doGet(HttpServletRequest request,
                        HttpServletResponse response) {
      processResponse(response, "", HttpServletResponse.SC_NOT_FOUND, logger);
   }

   // TODO: This is not a proper way to authenticate the user. We have plans to
   // authenticate every user via CSP, however that integration will take time
   // and till then some way of authentication is needed. Hence, we have added
   // this temporary (and not very secure) login feature. Remove this and
   // authenticate every user with CSP as soon as possible

   @Override
   protected void doPost(HttpServletRequest request,
                         HttpServletResponse response) {
      JSONParser parser = new JSONParser();
      String responseString;
      int responseStatus;
      try {
         JSONObject requestJSON
            = (JSONObject) parser.parse(APIHelper.getRequestBody(request));
         String uri = request.getRequestURI();
         if (uri.endsWith("/")) {
            uri = uri.substring(0, uri.length() - 1);
         }
         String tokens[] = uri.split("/");
         // URI is /api/login/<userID>
         if (tokens[3].equals("login")) {
            boolean successful
               = prm.loginUser(tokens[4],
                               (String) requestJSON.get(PASSWORD_LABEL));
            if (successful) {
               responseStatus = HttpServletResponse.SC_OK;
               responseString = new JSONObject().toJSONString();
            } else {
               responseStatus = HttpServletResponse.SC_FORBIDDEN;
               responseString = new JSONObject().toJSONString();
            }
         } else {
            responseStatus = HttpServletResponse.SC_NOT_FOUND;
            responseString = new JSONObject().toJSONString();
         }
      } catch (ParseException | IOException | UserModificationException e) {
         responseStatus = HttpServletResponse.SC_BAD_REQUEST;
         responseString = APIHelper.errorJSON(e.getMessage()).toJSONString();
      }
      processResponse(response, responseString, responseStatus, logger);
   }

   @Override
   protected JSONAware parseToJSON(Athena.AthenaResponse athenaResponse) {
      return null;
   }
}

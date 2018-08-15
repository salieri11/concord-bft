/**
 * <p>
 * Copyright 2018 VMware, all rights reserved.
 * </p>
 *
 */

package Servlets;

import static profiles.UsersAPIMessage.EMAIL_LABEL;
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
         // URI is /api/login
         if (requestJSON.containsKey(EMAIL_LABEL)
            && requestJSON.containsKey(PASSWORD_LABEL)) {
            boolean successful
               = prm.loginUser((String) requestJSON.get(EMAIL_LABEL),
                               (String) requestJSON.get(PASSWORD_LABEL));
            if (successful) {
               responseStatus = HttpServletResponse.SC_OK;
               responseString = new JSONObject().toJSONString();
            } else {
               responseStatus = HttpServletResponse.SC_FORBIDDEN;
               responseString = new JSONObject().toJSONString();
            }
         } else {
            responseString
               = APIHelper.errorJSON("email or password " + "field missing")
                          .toJSONString();
            responseStatus = HttpServletResponse.SC_BAD_REQUEST;
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

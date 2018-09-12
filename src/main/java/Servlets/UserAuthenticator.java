/**
 * <p>
 * Copyright 2018 VMware, all rights reserved.
 * </p>
 *
 */

package Servlets;

import static services.profiles.UsersAPIMessage.EMAIL_LABEL;
import static services.profiles.UsersAPIMessage.PASSWORD_LABEL;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.json.simple.JSONAware;
import org.json.simple.JSONObject;
import org.json.simple.parser.JSONParser;
import org.json.simple.parser.ParseException;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.stereotype.Controller;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestMethod;

import com.vmware.athena.Athena;

import services.profiles.ProfilesRegistryManager;
import services.profiles.UserModificationException;
import services.profiles.User;

/**
 * A servlet for handling the user authentication flow of helen. This servlet is
 * just added for temporary usage. Actual authentication will be done with CSP.
 * Do NOT rely on this servlet for primary authentication method.
 */
@Controller
public class UserAuthenticator extends BaseServlet {

   private static final Logger logger
      = LogManager.getLogger(UserAuthenticator.class);

   @Autowired
   private ProfilesRegistryManager prm;

   // TODO: This is not a proper way to authenticate the user. We have plans to
   // authenticate every user via CSP, however that integration will take time
   // and till then some way of authentication is needed. Hence, we have added
   // this temporary (and not very secure) login feature. Remove this and
   // authenticate every user with CSP as soon as possible
   @RequestMapping(method = RequestMethod.POST, path = "/api/login")
   protected ResponseEntity<JSONAware> doPost(@RequestBody String requestBody) {
      JSONParser parser = new JSONParser();
      HttpStatus responseStatus;
      JSONObject responseJSON;
      try {
         JSONObject requestJSON = (JSONObject) parser.parse(requestBody);
         if (requestJSON.containsKey(EMAIL_LABEL)
            && requestJSON.containsKey(PASSWORD_LABEL)) {
            JSONObject user
               = prm.loginUser((String) requestJSON.get(EMAIL_LABEL),
                               (String) requestJSON.get(PASSWORD_LABEL));

            if (user.get("isAuthenticated") == "true") {
               responseStatus = HttpStatus.OK;
               responseJSON = user;
            } else {
               responseStatus = HttpStatus.FORBIDDEN;
               responseJSON = new JSONObject();
            }
         } else {
            responseJSON
               = APIHelper.errorJSON("email or password " + "field missing");
            responseStatus = HttpStatus.BAD_REQUEST;
         }
      } catch (ParseException | UserModificationException e) {
         responseStatus = HttpStatus.BAD_REQUEST;
         responseJSON = APIHelper.errorJSON(e.getMessage());
      }

      return new ResponseEntity<>(responseJSON,
                                  standardHeaders,
                                  responseStatus);
   }

   @Override
   protected JSONAware parseToJSON(Athena.AthenaResponse athenaResponse) {
      return null;
   }
}

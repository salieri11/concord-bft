/**
 * <p>
 * Copyright 2018 VMware, all rights reserved.
 * </p>
 *
 */

package Servlets;

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
import org.springframework.web.bind.annotation.*;

import com.vmware.athena.Athena;

import services.profiles.*;

/**
 * A servlet which manages all GET/POST/PATCH requests related to user
 * management API of helen
 */
@Controller
public class AgreementServlet extends BaseServlet {

   private static final Logger logger
      = LogManager.getLogger(AgreementServlet.class);

   @Autowired
   private AgreementsRegistryManager arm;

   public AgreementServlet() {}

   @RequestMapping(path = "/api/agreements/{id}", method = RequestMethod.GET)
   public ResponseEntity<JSONAware>
          getAgreementFromID(@PathVariable("id") String ID) {
      JSONObject result = arm.getAgreementWithID(ID);
      if (result.isEmpty()) {
         result.put("error", "Agreement not found");
         return new ResponseEntity<>(result,
                                     standardHeaders,
                                     HttpStatus.NOT_FOUND);
      } else {
         return new ResponseEntity<>(result, standardHeaders, HttpStatus.OK);
      }
   }

   @RequestMapping(path = "/api/agreements/{id}", method = RequestMethod.PATCH)
   public ResponseEntity<JSONAware>
          doPatch(@PathVariable(name = "id") String id,
                  @RequestBody String requestBody) {
      HttpStatus responseStatus;
      JSONObject responseJson;

      try {
         JSONParser parser = new JSONParser();
         JSONObject requestJson = (JSONObject) parser.parse(requestBody);

         arm.updateAgreement(id, requestJson);
         logger.debug("Agreement accepted");
         responseJson = new JSONObject();
         responseStatus = HttpStatus.OK;

      } catch (ParseException e) {
         responseJson = APIHelper.errorJSON(e.getMessage());
         responseStatus = HttpStatus.BAD_REQUEST;
      }

      return new ResponseEntity<>(responseJson,
                                  standardHeaders,
                                  responseStatus);
   }

   @Override
   protected JSONAware parseToJSON(Athena.AthenaResponse athenaResponse) {
      throw new UnsupportedOperationException("parseToJSON method is not "
         + "supported in ProfileManager class");
   }
}

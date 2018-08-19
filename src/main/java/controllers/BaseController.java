/**
 * This is the base class for all API based servlets.
 */
package controllers;

import org.apache.logging.log4j.LogManager;
import org.json.simple.JSONAware;
import org.json.simple.JSONObject;
import org.springframework.http.HttpHeaders;
import org.springframework.http.HttpStatus;
import org.springframework.http.MediaType;
import org.springframework.http.ResponseEntity;

import com.vmware.athena.Athena;

import configurations.ConfigurationFactory;
import configurations.ConfigurationFactory.ConfigurationType;
import configurations.IConfiguration;
import connections.AthenaConnectionException;
import connections.AthenaConnectionPool;
import connections.IAthenaConnection;
import utils.APIHelper;
import utils.AthenaHelper;

public abstract class BaseController {

   protected static final long serialVersionUID = 1L;

   protected IConfiguration _conf;
   protected HttpHeaders standardHeaders;

   protected BaseController() {
      _conf = ConfigurationFactory.getConfiguration(ConfigurationType.File);
      standardHeaders = new HttpHeaders();
      standardHeaders.setContentType(MediaType.APPLICATION_JSON);
      standardHeaders.set("Content-Transfer-Encoding", "UTF-8");
   }

   protected abstract JSONAware
             parseToJSON(Athena.AthenaResponse athenaResponse);

   /**
    * Process get request
    *
    * @param req
    *           - Athena request object
    */
   protected Athena.AthenaResponse
             forwardToAthena(Athena.AthenaRequest req) throws AthenaConnectionException {
      IAthenaConnection conn = null;
      Athena.AthenaResponse athenaResponse;
      try {
         conn = AthenaConnectionPool.getInstance().getConnection();
         if (conn == null) {
            throw new AthenaConnectionException("Unable to get athena "
               + "connection");
         }

         boolean res = AthenaHelper.sendToAthena(req, conn, _conf);
         if (!res) {
            throw new AthenaConnectionException("Sending to athena failed");
         }

         // receive response from Athena
         athenaResponse = AthenaHelper.receiveFromAthena(conn);
         if (athenaResponse == null) {
            throw new AthenaConnectionException("Athena sent invalid response");
         }
         return athenaResponse;
      } catch (Exception e) {
         throw new AthenaConnectionException("Athena internal error: "
            + e.getMessage());
      } finally {
         AthenaConnectionPool.getInstance().putConnection(conn);
      }
   }

   protected ResponseEntity<JSONAware>
             buildHelenResponse(Athena.AthenaResponse athenaResponse) {
      JSONAware respObject;
      HttpStatus status;
      if (athenaResponse.getErrorResponseCount() == 0) {
         respObject = parseToJSON(athenaResponse);
         status = respObject == null ? HttpStatus.INTERNAL_SERVER_ERROR
            : HttpStatus.OK;
      } else {
         Athena.ErrorResponse errorResp = athenaResponse.getErrorResponse(0);

         String message = errorResp.getDescription();
         JSONObject obj = new JSONObject();
         obj.put("error", message);
         respObject = obj;
         // trying to be a little fancy with status codes here, but we should
         // probably change ErrorResponse to include a "code" field, so Athena
         // can signal exactly what kind of error happened
         if (message.contains("not found")) {
            // block/transaction not found
            status = HttpStatus.NOT_FOUND;
         } else if (message.contains("Missing")
            || message.contains("request")) {
            // Missing required parameter
            // Invalid ... request
            status = HttpStatus.BAD_REQUEST;
         } else {
            status = HttpStatus.INTERNAL_SERVER_ERROR;
         }
      }
      return new ResponseEntity<>(respObject, standardHeaders, status);
   }

   protected ResponseEntity<JSONAware>
   sendToAthenaAndBuildHelenResponse(Athena.AthenaRequest athenaRequest) {
      try {
         Athena.AthenaResponse athenaResponse = forwardToAthena(athenaRequest);
         return buildHelenResponse(athenaResponse);
      } catch (AthenaConnectionException ace) {
         LogManager.getLogger(BaseController.class)
                 .warn("Athena Exception: ", ace);
         return new ResponseEntity<>(APIHelper.errorJSON(ace.getMessage()),
                                     standardHeaders,
                                     HttpStatus.INTERNAL_SERVER_ERROR);
      }
   }
}

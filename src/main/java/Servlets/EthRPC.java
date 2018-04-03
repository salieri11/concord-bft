/**
 * url endpoint : /api/athena/eth
 *
 * GET:
 * Used to list available RPC methods.
 * A list of currently exposed Eth RPC methods is read from the config file
 * and returned to the client.
 *
 * POST:
 * Used to call the named method.
 * An EthRPCExecute request is sent to Athena and to parse the responses into
 * JSON. A TCP socket connection is made to Athena and requests and responses
 * are encoded in the Google Protocol Buffer format.
 *
 * Athena, by default, runs on port 5458.
 * TODO : Handle the case of no/incorrect response from Athena
 */
package Servlets;

import com.google.protobuf.ByteString;
import com.vmware.athena.*;
import com.vmware.athena.Athena.EthRPCExecuteResponse;
import io.undertow.util.StatusCodes;
import java.io.IOException;
import java.util.ArrayList;
import java.util.stream.Collectors;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import org.apache.log4j.Logger;
import org.json.simple.JSONArray;
import org.json.simple.JSONObject;
import org.json.simple.parser.JSONParser;
import org.json.simple.parser.ParseException;

/**
 * Servlet class.
 */
public final class EthRPC extends BaseServlet {
   private static final long serialVersionUID = 1L;
   private static Logger logger = Logger.getLogger(EthRPC.class);
   private JSONArray rpcList;
   
   public EthRPC() throws ParseException {
      super();
      JSONParser p = new JSONParser();
      rpcList = (JSONArray)p.parse(_conf.getStringValue("EthRPCList"));
   }
   
   /**
    * Services the Get request for listing currently exposed Eth RPCs.
    *  Retrieves the list from the configurations file 
    *  and returns it to the client.
    *
    * @param request
    *           The request received by the servlet
    * @param response
    *           The response object used to respond to the client
    * @throws IOException
    */
   protected void doGet(final HttpServletRequest request,
            final HttpServletResponse response) throws IOException {

      if (rpcList == null) {
         logger.error("Configurations not read.");
         processResponse(response,
               "error", 
                StatusCodes.INTERNAL_SERVER_ERROR,
                logger);
      }

      processResponse(response, 
            rpcList.toJSONString(),
            StatusCodes.OK,
            logger);
   }

   /**
    * Services the Post request for executing the specified method.
    *  Retrieves
    * the request parameters and forwards the request to Athena
    *  for execution.
    * Receives a response from Athena and forwards it to the client.
    *
    * @param request
    *           The request received by the servlet
    * @param response
    *           The response object used to respond to the client
    * @throws IOException
    */
   protected void doPost(final HttpServletRequest request,
            final HttpServletResponse response) throws IOException {

      // Retrieve the request fields
      JSONObject requestParams = null;
      Long id = null;
      String jsonRpc = null;
      String method = null;
      JSONArray params = null;

      JSONParser parser = new JSONParser();
      try {
         // Retrieve the parameters from the request body
         String paramString = request.getReader().lines()
                  .collect(Collectors.joining(System.lineSeparator()));
         requestParams = (JSONObject) parser.parse(paramString);
         if (requestParams == null) {
            logger.error("Invalid request : Parameters should be in the "
                     + "request body and in a JSON object format");
            processResponse(response,
                           "error", 
                            StatusCodes.BAD_REQUEST,
                            logger);
            return;
         }
      } catch (ParseException e) {
         logger.error("Invalid request", e);
         processResponse(response,
               "error", 
                StatusCodes.BAD_REQUEST,
                logger);
         return;
      }

      try {
         id = (Long) requestParams.get("id");
      } catch (NumberFormatException e) {
         logger.error("Invalid request parameter : id", e);
         processResponse(response,
               "error", 
                StatusCodes.BAD_REQUEST,
                logger);
         return;
      }

      jsonRpc = (String) requestParams.get("jsonrpc");
      if (jsonRpc == null) {
         logger.error("Invalid request parameter : jsonrpc");
         processResponse(response,
               "error", 
                StatusCodes.BAD_REQUEST,
                logger);
         return;
      }

      method = (String) requestParams.get("method");
      if (method == null) {
         logger.error("Invalid request parameter : method");
         processResponse(response,
               "error", 
                StatusCodes.BAD_REQUEST,
                logger);
         return;
      }
      params = (JSONArray) requestParams.get("params");

      if (params.size() < 1) {
         logger.error("Invalid request parameter : params");
         processResponse(response,
               "error", 
                StatusCodes.BAD_REQUEST,
                logger);
         return;
      }

      // Convert list of hex strings to list of binary strings
      // Athena expects params in binary strings
      ArrayList<ByteString> paramBytes = new ArrayList<>();
      try {
         for (Object param : params) {
            paramBytes.add(APIHelper.hexStringToBinary((String) param));
         }
      } catch (Exception e) {
         logger.error("Invalid request parameter : params");
         processResponse(response,
               "error", 
                StatusCodes.BAD_REQUEST,
                logger);
         return;
      }

      // Construct an ethrpcexecute request object.
      Athena.EthRPCExecuteRequest ethRpcExecuteRequestObj =
            Athena.EthRPCExecuteRequest
               .newBuilder()
               .setId(id)
               .setJsonrpc(jsonRpc)
               .setMethod(method)
               .addAllParams(paramBytes)
               .build();

      // Envelope the request object into an athena request object.
      final Athena.AthenaRequest athenarequestObj =
            Athena.AthenaRequest
               .newBuilder()
               .setEthRpcExecuteRequest(ethRpcExecuteRequestObj)
               .build();

      processGet(athenarequestObj, response, logger);
   }

   /**
    * Parses the Protocol Buffer response from Athena and converts 
    * it into JSON.
    *
    * @param athenaResponse
    *           Protocol Buffer object containing Athena's reponse
    * @return Response in JSON format
    */
   @SuppressWarnings("unchecked")
   @Override
   protected JSONObject parseToJSON(Athena.AthenaResponse athenaResponse)
   {

      // Extract the ethrpcexecute response from 
      // the athena reponse envelope.
      EthRPCExecuteResponse ethRpcExecuteResponse = athenaResponse
               .getEthRpcExecuteResponse();

      // Construct the response JSON object.
      JSONObject responseJson = new JSONObject();
      responseJson.put("id", ethRpcExecuteResponse.getId());
      responseJson.put("jsonrpc", ethRpcExecuteResponse.getJsonrpc());

      /*
       * Convert the binary string received from Athena into
       *  a hex string for
       * responding to the client
       */
      String resultString = APIHelper
               .binaryStringToHex(ethRpcExecuteResponse.getResult());
      responseJson.put("result", resultString);

      responseJson.put("error", ethRpcExecuteResponse.getError());
      return responseJson;
   }
}

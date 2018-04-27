/**
 * url endpoint : /api/athena/eth
 *
 * GET: Used to list available RPC methods. A list of currently exposed Eth RPC
 * methods is read from the config file and returned to the client.
 *
 * POST: Used to call the named method. An EthRPCExecute request is sent to
 * Athena and to parse the responses into JSON. A TCP socket connection is made
 * to Athena and requests and responses are encoded in the Google Protocol
 * Buffer format.
 *
 */
package Servlets;

import Servlets.EthRPCHandlers.*;
import com.vmware.athena.*;
import com.vmware.athena.Athena.AthenaResponse;
import com.vmware.athena.Athena.ErrorResponse;

import connections.AthenaConnectionPool;
import connections.IAthenaConnection;

import io.undertow.util.StatusCodes;
import java.io.IOException;
import java.util.stream.Collectors;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import org.apache.log4j.Logger;
import org.json.simple.JSONArray;
import org.json.simple.JSONAware;
import org.json.simple.JSONObject;
import org.json.simple.parser.JSONParser;
import org.json.simple.parser.ParseException;

/**
 * Servlet class.
 */
public final class EthDispatcher extends BaseServlet {
   private static final long serialVersionUID = 1L;
   private static Logger logger = Logger.getLogger(EthDispatcher.class);
   public static long netVersion;
   public static boolean netVersionSet = false;
   private JSONArray rpcList;
   private String jsonRpc;

   public EthDispatcher() throws ParseException {
      super();
      JSONParser p = new JSONParser();
      try {
         rpcList = (JSONArray) p.parse(_conf.getStringValue("EthRPCList"));
         jsonRpc = _conf.getStringValue("JSONRPC");
      } catch (Exception e) {
         logger.error("Failed to read RPC information from config file", e);
      }
   }

   /**
    * Services the Get request for listing currently exposed Eth RPCs. Retrieves
    * the list from the configurations file and returns it to the client.
    *
    * @param request
    *           The request received by the servlet
    * @param response
    *           The response object used to respond to the client
    * @throws IOException
    */
   protected void doGet(final HttpServletRequest request,
                        final HttpServletResponse response) throws IOException {
      String responseString = rpcList.toJSONString();
      int statusCode = StatusCodes.OK;
      if (rpcList == null) {
         logger.error("Configurations not read.");
         responseString = (new JSONArray()).toJSONString();
         statusCode = HttpServletResponse.SC_SERVICE_UNAVAILABLE;
      }
      processResponse(response, responseString, statusCode, logger);
   }

   /**
    * Services the Post request for executing the specified methods. Retrieves
    * the request parameters and calls the dispatch function. Builds the
    * response for sending to client.
    *
    * @param request
    *           The request received by the servlet
    * @param response
    *           The response object used to respond to the client
    * @throws IOException
    */
   @SuppressWarnings("unchecked")
   protected void
             doPost(final HttpServletRequest request,
                    final HttpServletResponse response) throws IOException {

      // Retrieve the request fields
      JSONArray batchRequest = null;
      JSONArray batchResponse = new JSONArray();
      JSONParser parser = new JSONParser();
      String responseString = null;
      boolean isBatch = false;
      try {
         // Retrieve the parameters from the request body
         String paramString
            = request.getReader()
                     .lines()
                     .collect(Collectors.joining(System.lineSeparator()));
         logger.debug("Request Parameters: " + paramString);

         // If we receive a single request, add it to a JSONArray for the sake
         // of uniformity.
         if (paramString.startsWith("[")) {
            isBatch = true;
            batchRequest = (JSONArray) parser.parse(paramString);
            if (batchRequest == null || batchRequest.size() == 0) {
               throw new Exception("Invalid request");
            }
         } else {
            batchRequest = new JSONArray();
            batchRequest.add((JSONObject) parser.parse(paramString));
         }

         for (Object params : batchRequest) {
            JSONObject requestParams = (JSONObject) params;

            // Dispatch requests to the corresponding handlers
            batchResponse.add(dispatch(response, requestParams));
         }
         if (isBatch) {
            responseString = batchResponse.toJSONString();
         } else {
            responseString = (String) batchResponse.get(0);
         }
      } catch (ParseException e) {
         logger.error("Invalid request", e);
         responseString = errorMessage("Unable to parse request", -1, jsonRpc);
      } catch (Exception e) {
         logger.error(APIHelper.exceptionToString(e));
         responseString = errorMessage(e.getMessage(), -1, jsonRpc);
      }
      processResponse(response,
                      responseString,
                      HttpServletResponse.SC_OK,
                      logger);
   }

   private String dispatch(final HttpServletResponse response,
                           JSONObject requestJson) throws Exception {
      // Default initialize variables, so that if exception is thrown
      // while initializing the variables error message can be constructed
      // with default values.
      long id = -1;
      String ethMethodName;
      AbstractEthRPCHandler handler = null;
      boolean isLocal = false;
      String responseString;
      AthenaResponse athenaResponse = null;

      // Dispatch to appropriate handlers
      try {
         ethMethodName = getEthMethodName(requestJson);
         id = getEthRequestId(requestJson);
         if (ethMethodName.equals(_conf.getStringValue("SendTransaction_Name"))
            || ethMethodName.equals(_conf.getStringValue("Call_Name"))) {
            handler = new EthSendTxHandler();
         } else if (ethMethodName.equals(_conf.getStringValue("NewAccount_Name"))) {
            handler = new EthNewAccountHandler();
         } else if (ethMethodName.equals(_conf.getStringValue("GetTransactionReceipt_Name"))) {
            handler = new EthGetTxReceiptHandler();
         } else if (ethMethodName.equals(_conf.getStringValue("GetStorageAt_Name"))) {
            handler = new EthGetStorageAtHandler();
         } else if (ethMethodName.equals(_conf.getStringValue("GetCode_Name"))) {
            handler = new EthGetCodeHandler();
         } else if (ethMethodName.equals(_conf.getStringValue("NewFilter_Name"))
            || ethMethodName.equals(_conf.getStringValue("NewBlockFilter_Name"))
            || ethMethodName.equals(_conf.getStringValue("NewPendingTransactionFilter_Name"))
            || ethMethodName.equals(_conf.getStringValue("FilterChange_Name"))
            || ethMethodName.equals(_conf.getStringValue("UninstallFilter_Name"))) {
            handler = new EthFilterHandler();
         } else if (ethMethodName.equals(_conf.getStringValue("Web3SHA3_Name"))
            || ethMethodName.equals(_conf.getStringValue("RPCModules_Name"))
            || ethMethodName.equals(_conf.getStringValue("Coinbase_Name"))
            || ethMethodName.equals(_conf.getStringValue("ClientVersion_Name"))
            || ethMethodName.equals(_conf.getStringValue("Mining_Name"))
            || ethMethodName.equals(_conf.getStringValue("NetVersion_Name"))
            || ethMethodName.equals(_conf.getStringValue("Accounts_Name"))) {
            handler = new EthLocalResponseHandler();
            isLocal = true;
         } else {
            throw new Exception("Invalid method name.");
         }

         if (!isLocal) {
            Athena.EthRequest ethRequest = handler.buildRequest(requestJson);

            Athena.AthenaRequest athenaRequest
               = Athena.AthenaRequest.newBuilder()
                                     .addEthRequest(ethRequest)
                                     .build();

            athenaResponse
               = communicateWithAthena(athenaRequest, response, logger);

            // If there is an error reported by Athena
            if (athenaResponse.getErrorResponseCount() > 0) {
               ErrorResponse errResponse = athenaResponse.getErrorResponse(0);
               responseString = "Error received from athena";
               if (errResponse.hasDescription()) {
                  responseString = errResponse.getDescription();
               }
            } else {
               responseString
                  = handler.buildResponse(athenaResponse.getEthResponse(0),
                                          requestJson)
                           .toJSONString();
            }
         } else {
            // In local request we don't have valid eth resposne from
            // athena. Just pass null.
            responseString
               = handler.buildResponse(null, requestJson).toJSONString();
         }
      } catch (Exception e) {
         logger.error(APIHelper.exceptionToString(e));
         responseString = errorMessage(e.getMessage(), id, jsonRpc);
      }
      logger.debug("Response string: " + responseString);
      return responseString;
   }

   private AthenaResponse communicateWithAthena(Athena.AthenaRequest req,
                                                HttpServletResponse response,
                                                Logger log) {
      IAthenaConnection conn = null;
      Athena.AthenaResponse athenaResponse = null;
      try {
         conn = AthenaConnectionPool.getInstance().getConnection();
         if (conn == null) {
            processResponse(response,
                            errorMessage("Error communicating with athena",
                                         req.getEthRequest(0).getId(),
                                         jsonRpc),
                            HttpServletResponse.SC_OK,
                            log);
            return null;
         }

         boolean res = AthenaHelper.sendToAthena(req, conn, _conf);
         if (!res) {
            processResponse(response,
                            errorMessage("Error communicating with athena",
                                         req.getEthRequest(0).getId(),
                                         jsonRpc),
                            HttpServletResponse.SC_OK,
                            log);
            return null;
         }

         // receive response from Athena
         athenaResponse = AthenaHelper.receiveFromAthena(conn);
         if (athenaResponse == null) {
            processResponse(response,
                            errorMessage("Error reading response from athena",
                                         req.getEthRequest(0).getId(),
                                         jsonRpc),
                            HttpServletResponse.SC_OK,
                            log);
            return null;
         }
      } catch (Exception e) {
         logger.error("General exception communicating with athena: ", e);
         processResponse(response,
                         errorMessage("Error communicating with athena",
                                      req.getEthRequest(0).getId(),
                                      jsonRpc),
                         HttpServletResponse.SC_OK,
                         log);
         return null;
      } finally {
         AthenaConnectionPool.getInstance().putConnection(conn);
      }

      return athenaResponse;
   }

   @SuppressWarnings("unchecked")
   public static String errorMessage(String message, long id, String jsonRpc) {
      JSONObject responseJson = new JSONObject();
      responseJson.put("id", id);
      responseJson.put("jsonprc", jsonRpc);

      JSONObject error = new JSONObject();
      error.put("message", message);
      responseJson.put("error", error);

      return responseJson.toJSONString();
   }

   public static String
          getEthMethodName(JSONObject ethRequestJson) throws Exception {
      try {
         return (String) ethRequestJson.get("method");
      } catch (Exception e) {
         throw new Exception("invalid method parameter in request.", e);
      }
   }

   public static long
          getEthRequestId(JSONObject ethRequestJson) throws Exception {
      try {
         return (long) ethRequestJson.get("id");
      } catch (Exception e) {
         throw new Exception("invalid id parameter in request.", e);
      }
   }

   @Override
   protected JSONAware parseToJSON(AthenaResponse athenaResponse) {
      return null;
   }
}

package Servlets;

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

import com.vmware.athena.Athena;
import com.vmware.athena.Athena.AthenaResponse;
import com.vmware.athena.Athena.ErrorResponse;

import Servlets.EthRPCHandlers.*;
import connections.AthenaConnectionPool;
import connections.IAthenaConnection;
import io.undertow.util.StatusCodes;

/**
 * <p>
 * Copyright 2018 VMware, all rights reserved.
 * </p>
 *
 * <p>
 * url endpoint : /api/athena/eth
 * </p>
 *
 * <p>
 * GET: Used to list available RPC methods. A list of currently exposed Eth RPC
 * methods is read from the config file and returned to the client.
 * </p>
 *
 * <p>
 * POST: Used to execute the specified method. Request and response construction
 * are handled by the appropriate handlers. A TCP socket connection is made to
 * Athena and requests and responses are encoded in the Google Protocol Buffer
 * format. Also supports a group of requests.
 * </p>
 */
public final class EthDispatcher extends BaseServlet {
   private static final long serialVersionUID = 1L;
   public static long netVersion;
   public static boolean netVersionSet = false;
   private static Logger logger = Logger.getLogger(EthDispatcher.class);
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
    * Constructs the response in case of error.
    *
    * @param message
    *           Error message
    * @param id
    *           Request Id
    * @param jsonRpc
    *           RPC version
    * @return Error message string
    */
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

   /**
    * Extracts the RPC method name from the request JSON
    *
    * @param ethRequestJson
    *           Request JSON
    * @return Method name
    * @throws Exception
    */
   public static String getEthMethodName(JSONObject ethRequestJson)
      throws EthRPCHandlerException {
      if (ethRequestJson.containsKey("method")) {
         if (ethRequestJson.get("method") instanceof String) {
            return (String) ethRequestJson.get("method");
         } else {
            throw new EthRPCHandlerException("method must be a string");
         }
      } else {
         throw new EthRPCHandlerException("request must contain a method");
      }
   }

   /**
    * Extracts the Request Id from the request JSON
    *
    * @param ethRequestJson
    *           Request JSON
    * @return Request id
    * @throws Exception
    */
   public static long getEthRequestId(JSONObject ethRequestJson)
      throws EthRPCHandlerException {
      if (ethRequestJson.containsKey("id")) {
         if (ethRequestJson.get("id") instanceof Number) {
            return ((Number)ethRequestJson.get("id")).longValue();
         } else {
            throw new EthRPCHandlerException("id must be a number");
         }
      } else {
         throw new EthRPCHandlerException("request must contain an id");
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
    * Services the POST request for executing the specified methods. Retrieves
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
            batchResponse.add(parser.parse(dispatch(requestParams)));
         }
         if (isBatch) {
            responseString = batchResponse.toJSONString();
         } else {
            responseString = ((JSONObject) batchResponse.get(0)).toJSONString();
         }
      } catch (ParseException e) {
         logger.error("Invalid request", e);
         responseString = errorMessage("Unable to parse request", -1, jsonRpc);
      } catch (Exception e) {
         logger.error(APIHelper.exceptionToString(e));
         responseString = errorMessage(e.getMessage(), -1, jsonRpc);
      }
      logger.debug("Response: " + responseString);
      processResponse(response,
                      responseString,
                      HttpServletResponse.SC_OK,
                      logger);
   }

   /**
    * Creates the appropriate handler object and calls its functions to
    * construct an AthenaRequest object. Sends this request to Athena and
    * converts its response into a format required by the user.
    *
    * @param requestJson
    *           Request parameters
    * @return Response for user
    * @throws Exception
    */
   String dispatch(JSONObject requestJson) throws Exception {
      // Default initialize variables, so that if exception is thrown
      // while initializing the variables error message can be constructed
      // with default values.
      long id = -1;
      String ethMethodName;
      AbstractEthRPCHandler handler = null;
      boolean isLocal = false;
      String responseString;
      AthenaResponse athenaResponse = null;

      // Create object of the suitable handler based on the method specified in
      // the request
      try {
         ethMethodName = getEthMethodName(requestJson);
         id = getEthRequestId(requestJson);
         if (ethMethodName.equals(_conf.getStringValue("SendTransaction_Name"))
             || ethMethodName.equals(_conf.getStringValue("SendRawTransaction_Name"))
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
         } else if (ethMethodName.equals(_conf.getStringValue("GetTransactionCount_Name"))) {
            handler = new EthGetTransactionCountHandler();
         } else if (ethMethodName.equals(_conf.getStringValue
                 ("GetBlockByHash_Name")) ||
                 ethMethodName.equals(_conf.getStringValue
                         ("GetBlockByNumber_Name"))) {
            handler = new EthGetBlockHandler();
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
            || ethMethodName.equals(_conf.getStringValue("Accounts_Name"))
            || ethMethodName.equals(_conf.getStringValue("GasPrice_Name"))) {
            handler = new EthLocalResponseHandler();
            isLocal = true;
         } else if (ethMethodName.equals(_conf.getStringValue
                 ("BlockNumber_Name"))) {
            handler = new EthBlockNumberHandler();
         }
         else {
            throw new Exception("Invalid method name.");
         }

         if (!isLocal) {
            Athena.AthenaRequest.Builder athenaRequestBuilder
               = Athena.AthenaRequest.newBuilder();
            handler.buildRequest(athenaRequestBuilder, requestJson);
            athenaResponse
               = communicateWithAthena(athenaRequestBuilder.build());
            // If there is an error reported by Athena
            if (athenaResponse.getErrorResponseCount() > 0) {
               ErrorResponse errResponse = athenaResponse.getErrorResponse(0);
               if (errResponse.hasDescription()) {
                  responseString = errorMessage(
                     errResponse.getDescription(), id, jsonRpc);
               } else {
                  responseString = errorMessage(
                     "Error received from athena", id, jsonRpc);
               }
            } else {
               responseString
                  = handler.buildResponse(athenaResponse, requestJson)
                           .toJSONString();
            }
         }
         // There are some RPC methods which are handled locally by Helen. No
         // need to talk to Athena for these cases.
         else {
            // In local request we don't have valid eth resposne from
            // athena. Just pass null.
            responseString
               = handler.buildResponse(null, requestJson).toJSONString();
         }
      } catch (Exception e) {
         logger.error(APIHelper.exceptionToString(e));
         responseString = errorMessage(e.getMessage(), id, jsonRpc);
      }
      return responseString;
   }

   /**
    * Sends an AthenaRequest to Athena and receives Athena's response.
    *
    * @param req
    *           AthenaRequest object
    * @return Response received from Athena
    */
   private AthenaResponse
           communicateWithAthena(Athena.AthenaRequest req) throws Exception {
      IAthenaConnection conn = null;
      Athena.AthenaResponse athenaResponse = null;
      try {
         conn = AthenaConnectionPool.getInstance().getConnection();
         if (conn == null) {
            throw new Exception(errorMessage("Error communicating with athena",
                                             req.getEthRequest(0).getId(),
                                             jsonRpc));
         }

         boolean res = AthenaHelper.sendToAthena(req, conn, _conf);
         if (!res) {
            throw new Exception(errorMessage("Error communicating with athena",
                                             req.getEthRequest(0).getId(),
                                             jsonRpc));
         }

         // receive response from Athena
         athenaResponse = AthenaHelper.receiveFromAthena(conn);
         if (athenaResponse == null) {
            throw new Exception(errorMessage("Error communicating with athena",
                                             req.getEthRequest(0).getId(),
                                             jsonRpc));
         }
      } catch (Exception e) {
         logger.error("General exception communicating with athena: ", e);
         throw e;
      } finally {
         AthenaConnectionPool.getInstance().putConnection(conn);
      }

      return athenaResponse;
   }

   /**
    * Not required for this Servlet as each handler builds its response object
    * separately.
    */
   @Override
   protected JSONAware parseToJSON(AthenaResponse athenaResponse) {
      throw new UnsupportedOperationException("parseToJSON method is not "
         + "supported in EthDispatcher Servlet");
   }
}

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

import com.google.protobuf.ByteString;
import com.vmware.athena.*;
import com.vmware.athena.Athena.ErrorResponse;
import com.vmware.athena.Athena.EthRequest.EthMethod;

import connections.AthenaConnectionPool;
import connections.IAthenaConnection;

import com.vmware.athena.Athena.EthResponse;
import com.vmware.athena.Athena.FilterResponse;
import com.vmware.athena.Athena.FilterRequest.*;

import io.undertow.util.StatusCodes;
import java.io.IOException;
import java.nio.charset.StandardCharsets;
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
   public static long netVersion;
   public static boolean netVersionSet = false;
   private JSONArray rpcList;
   private JSONObject rpcModules;
   private String jsonRpc;
   private String clientVersion;
   private String coinbase;
   private boolean isMining;

   private enum EthMethodName {
      SEND_TX,
      GET_TX_RECEIPT,
      GET_STORAGE_AT,
      CALL,
      NEW_ACCOUNT,
      NEW_FILTER,
      NEW_BLOCK_FILTER,
      NEW_PENDING_TRANSACTION_FILTER,
      FILTER_CHANGES,
      UNINSTALL_FILTER
   }

   public EthRPC() throws ParseException {
      super();
      JSONParser p = new JSONParser();
      try {
         rpcList = (JSONArray) p.parse(_conf.getStringValue("EthRPCList"));
         rpcModules
            = (JSONObject) (
               ((JSONArray) p.parse(_conf.getStringValue("RPCModules"))).get(0)
               );
         jsonRpc = _conf.getStringValue("JSONRPC");
         clientVersion = _conf.getStringValue("ClientVersion");
         isMining = _conf.getIntegerValue("Is_Mining") == 0 ? false : true;
         coinbase = _conf.getStringValue("Coinbase");
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

      if (rpcList == null) {
         logger.error("Configurations not read.");
         processResponse(response,
                         (new JSONArray()).toJSONString(),
                         HttpServletResponse.SC_SERVICE_UNAVAILABLE,
                         logger);
         return;
      }

      processResponse(response, rpcList.toJSONString(), StatusCodes.OK, logger);
   }

   /**
    * Services the Post request for executing the specified method. Retrieves
    * the request parameters and forwards the request to Athena for execution.
    * Receives a response from Athena and forwards it to the client.
    *
    * @param request
    *           The request received by the servlet
    * @param response
    *           The response object used to respond to the client
    * @throws IOException
    */
   protected void
             doPost(final HttpServletRequest request,
                    final HttpServletResponse response) throws IOException {

      // Retrieve the request fields
      JSONObject requestParams = null;
      Long id = null;
      String method = null;
      JSONArray params = null;
      String txHash = null;
      EthMethodName rpc = null;

      JSONParser parser = new JSONParser();
      try {
         // Retrieve the parameters from the request body
         String paramString
            = request.getReader()
                     .lines()
                     .collect(Collectors.joining(System.lineSeparator()));
         requestParams = (JSONObject) parser.parse(paramString);

         if (requestParams == null) {
            logger.error("Invalid request : Parameters should be in the "
               + "request body and in a JSON object format");
            errorResponse(response, "Unable to parse request", 0, logger);
            return;
         }
         logger.debug("Request Parameters: " + requestParams.toJSONString());

      } catch (ParseException e) {
         logger.error("Invalid request", e);
         errorResponse(response, "Unable to parse request", 0, logger);
         return;
      }

      try {
         id = (Long) requestParams.get("id");
      } catch (NumberFormatException e) {
         logger.error("Invalid request parameter : id", e);
         errorResponse(response, "'id' must be an integer", 0, logger);
         return;
      }

      method = (String) requestParams.get("method");
      if (method == null || method.trim().length() < 1) {
         logger.error("Invalid request parameter : method");
         errorResponse(response, "Invalid 'method' parameter", id, logger);
         return;
      }

      try {
         params = (JSONArray) requestParams.get("params");
      } catch (ClassCastException cse) {
         logger.error("Invalid request parameter : params");
         errorResponse(response, "'params' must be an array", id, logger);
         return;
      }

      Athena.EthRequest.Builder b = Athena.EthRequest.newBuilder();
      b.setId(id);

      // Set request fields as per the RPC
      try {
         // Convert an eth_call request to and eth_sendTransaction request
         if (method.equals(_conf.getStringValue("SendTransaction_Name"))
            || method.equals(_conf.getStringValue("Call_Name"))) {

            if (method.equals(_conf.getStringValue("SendTransaction_Name"))) {
               rpc = EthMethodName.SEND_TX;
               b.setMethod(EthMethod.SEND_TX);
            } else {
               rpc = EthMethodName.CALL;
               b.setMethod(EthMethod.CALL_CONTRACT);
            }

            String from = null, to = null, data = null, value = null;
            JSONObject obj = (JSONObject) params.get(0);

            if (obj.containsKey("from")) {
               from = (String) obj.get("from");
            }
            if (rpc.equals(EthMethodName.SEND_TX) && from == null) {
               logger.error("From field missing in params");
               errorResponse(response, "'from' must be specified", 0, logger);
               return;
            }

            if (obj.containsKey("to")) {
               to = (String) obj.get("to");
            }
            if (rpc.equals(EthMethodName.CALL) && to == null) {
               logger.error("To field missing in params");
               errorResponse(response, "'to' must be specified", 0, logger);
               return;
            }

            if (obj.containsKey("data")) {
               data = (String) obj.get("data");
            }
            if (obj.containsKey("value")) {
               value = (String) obj.get("value");
            }
            sendTransactionHandler(from, to, value, data, b);

         } else if (method.equals(_conf.getStringValue("NewAccount_Name"))) {
            rpc = EthMethodName.NEW_ACCOUNT;
            b.setMethod(EthMethod.NEW_ACCOUNT);
            String passphrase = (String) params.get(0);
            b.setData(ByteString.copyFrom(passphrase,
                                          StandardCharsets.UTF_8.name()));

         } else if (method.equals(
                       _conf.getStringValue("GetTransactionReceipt_Name"))) {
            rpc = EthMethodName.GET_TX_RECEIPT;
            b.setMethod(EthMethod.GET_TX_RECEIPT);
            txHash = (String) params.get(0);
            b.setData(APIHelper.hexStringToBinary(txHash));
         } else if (method.equals(_conf.getStringValue("GetStorageAt_Name"))) {
            rpc = EthMethodName.GET_STORAGE_AT;
            b.setMethod(EthMethod.GET_STORAGE_AT);
            b.setAddrTo(APIHelper.hexStringToBinary((String) params.get(0)));

            String p = (String) params.get(1);
            String s = padZeroes(p);
            b.setData(APIHelper.hexStringToBinary(s));
         } else if (method.equals(_conf.getStringValue("Web3SHA3_Name"))) {

            // Request should contain just one param value
            if (params.size() != 1) {
               logger.error("Invalid request parameter : params");
               errorResponse(response,
                             "'params' must contain only one element",
                             id,
                             logger);
               return;
            }
            localResponse(APIHelper.getKeccak256Hash((String) params.get(0)),
                          response,
                          id);
            return;
         } else if (method.equals(_conf.getStringValue("RPCModules_Name"))) {
            localResponse(rpcModules, response, id);
            return;
         } else if (method.equals(_conf.getStringValue("Coinbase_Name"))) {
            localResponse(coinbase, response, id);
            return;
         } else if (method.equals(_conf.getStringValue("ClientVersion_Name"))) {
            localResponse(clientVersion, response, id);
            return;
         } else if (method.equals(_conf.getStringValue("Mining_Name"))) {
            localResponse(isMining, response, id);
            return;
         } else if (method.equals(_conf.getStringValue("NetVersion_Name"))) {
            if(!EthRPC.netVersionSet){
               // The act of creating a connection retrieves info about athena.
               IAthenaConnection conn =
                  AthenaConnectionPool.getInstance().getConnection();
               if (conn == null){
                  String failureMsg = "Unable to connect to athena.";
                  logger.error(failureMsg);
                  errorResponse(response, failureMsg, id, logger);
                  return;
               }else{
                  EthRPC.netVersionSet = true;
               }
            }

            localResponse(EthRPC.netVersion, response, id);
            return;
         } else if (method.equals(_conf.getStringValue("Accounts_Name"))) {
            JSONArray usersJsonArr = new JSONArray();
            String usersStr = _conf.getStringValue("USERS");

            if (usersStr != null && !usersStr.trim().isEmpty()){
               String[] usersArr = usersStr.split(",");

               for(int i = 0; i < usersArr.length; i++){
                  usersJsonArr.add(usersArr[i]);
               }
            }

            localResponse(usersJsonArr, response, id);
            return;
         } else if (method.equals(_conf.getStringValue("NewFilter_Name"))) {
            //TODO: handle new filter
            logger.warn("eth_newFilter method is not implemented yet");
         } else if (method.equals(_conf.getStringValue("NewBlockFilter_Name"))) {
            rpc = EthMethodName.NEW_BLOCK_FILTER;
            b.setMethod(EthMethod.FILTER_REQUEST);
            // build FilterRequet
            Athena.FilterRequest.Builder fbuilder =
               Athena.FilterRequest.newBuilder();
            fbuilder.setType(FilterRequestType.NEW_BLOCK_FILTER);
            b.setFilterRequest(fbuilder.build());
         } else if (method.equals(
                       _conf.getStringValue(
                          "NewPendingTransactionFilter_Name"))) {
            //TODO: handle new pending transaction filter
            logger.warn("eth_newPendingTransactionFilter method is not" +
                        "implemented yet");
         } else if (method.equals(_conf.getStringValue("FilterChange_Name"))) {
            rpc = EthMethodName.FILTER_CHANGES;
            b.setMethod(EthMethod.FILTER_REQUEST);
            // build FilterRequet
            Athena.FilterRequest.Builder fbuilder =
               Athena.FilterRequest.newBuilder();
            fbuilder.setType(FilterRequestType.FILTER_CHANGE_REQUEST);
            fbuilder.setFilterId(
               APIHelper.hexStringToBinary((String) params.get(0)));
            b.setFilterRequest(fbuilder.build());
         } else if (method.equals(
                       _conf.getStringValue("UninstallFilter_Name"))) {
            rpc = EthMethodName.UNINSTALL_FILTER;
            b.setMethod(EthMethod.FILTER_REQUEST);
            // build FilterRequet
            Athena.FilterRequest.Builder fbuilder =
               Athena.FilterRequest.newBuilder();
            fbuilder.setType(FilterRequestType.UNINSTALL_FILTER);
            fbuilder.setFilterId(
               APIHelper.hexStringToBinary((String) params.get(0)));
            b.setFilterRequest(fbuilder.build());
         } else {
            logger.error("Invalid method name");
            errorResponse(response,
                          "Unknown method '" + method + "'",
                          id,
                          logger);
            return;
         }
      } catch (Exception e) {
         logger.error("Invalid request", e);
         errorResponse(response, "Invalid request", id, logger);
         return;
      }

      Athena.EthRequest athenaEthRequest = b.build();

      // Envelope the request object into an athena request object.
      final Athena.AthenaRequest athenarequestObj
         = Athena.AthenaRequest.newBuilder()
                               .addEthRequest(athenaEthRequest)
                               .build();
      processGet(rpc, txHash, athenarequestObj, response, logger);
   }

   /**
    * Transforms call and send_tx requests to send_tx request format
    *
    * @param from
    * @param to
    * @param value
    * @param data
    * @param b
    * @return
    * @throws Exception
    */
   private void
           sendTransactionHandler(String from, String to, String value,
                                  String data,
                                  Athena.EthRequest.Builder b) throws Exception {
      if (from != null) {
         ByteString fromAddr = APIHelper.hexStringToBinary(from);
         b.setAddrFrom(fromAddr);
      }

      if (to != null) {
         ByteString toAddr = APIHelper.hexStringToBinary(to);
         b.setAddrTo(toAddr);
      }

      if (data != null) {
         ByteString dataBytes = APIHelper.hexStringToBinary(data);
         b.setData(dataBytes);
      }
      if (value != null) {
         ByteString valueBytes = APIHelper.hexStringToBinary(padZeroes(value));
         b.setValue(valueBytes);
      }
   }

   /**
    * Used for RPCs for which Helen doesn't need to communicate with Athena
    *
    * @param data
    * @param response
    * @param id
    */
   @SuppressWarnings("unchecked")
   private void localResponse(Object data, HttpServletResponse response,
                              Long id) {
      JSONObject result = new JSONObject();
      result.put("id", id);
      result.put("jsonrpc", jsonRpc);
      result.put("result", data);
      processResponse(response,
                      result.toJSONString(),
                      HttpServletResponse.SC_OK,
                      logger);
   }

   /**
    * Pads zeroes to the hex string to ensure a uniform length of 64 hex
    * characters
    *
    * @param p
    * @return
    */
   private String padZeroes(String p) {
      int zeroes = 0;
      StringBuilder sb = new StringBuilder();
      if (p.startsWith("0x")) {
         p = p.substring(2);
      }
      if (p.length() < 64) {
         zeroes = 64 - p.length();
      }
      sb.append("0x");
      while (zeroes > 0) {
         sb.append("0");
         zeroes--;
      }
      sb.append(p);
      return sb.toString();
   }

   /**
    * Process get request
    *
    * @param req
    *           - Athena request object
    * @param response
    *           - HTTP servlet response object
    * @param log
    *           - specifies logger from servlet to use
    */
   @SuppressWarnings("unchecked")
   protected void processGet(EthMethodName method, String txHash,
                             Athena.AthenaRequest req,
                             HttpServletResponse response, Logger log) {
      JSONObject respObject = null;
      IAthenaConnection conn = null;
      Athena.AthenaResponse athenaResponse = null;
      try {
         conn = AthenaConnectionPool.getInstance().getConnection();
         if (conn == null) {
            errorResponse(response,
                          "Error communicating with athena",
                          req.getEthRequest(0).getId(),
                          log);
            return;
         }

         boolean res = AthenaHelper.sendToAthena(req, conn, _conf);
         if (!res) {
            errorResponse(response,
                          "Error communicating with athena",
                          req.getEthRequest(0).getId(),
                          log);
            return;
         }

         // receive response from Athena
         athenaResponse = AthenaHelper.receiveFromAthena(conn);
         if (athenaResponse == null) {
            errorResponse(response,
                          "Error reading response from athena",
                          req.getEthRequest(0).getId(),
                          log);
            return;
         }
      } catch (Exception e) {
         logger.error("General exception communicating with athena: ", e);
         errorResponse(response,
                       "Error communicating with athena",
                       req.getEthRequest(0).getId(),
                       log);
         return;
      } finally {
         AthenaConnectionPool.getInstance().putConnection(conn);
      }

      // If there is an error reported by Athena
      if (athenaResponse.getErrorResponseCount() > 0) {
         ErrorResponse errResponse = athenaResponse.getErrorResponse(0);
         String description = "Error received from athena";
         if (errResponse.hasDescription()) {
            description = errResponse.getDescription();
         }
         errorResponse(response,
                       description,
                       req.getEthRequest(0).getId(),
                       log);
         return;
      }

      respObject = parseToJSON(athenaResponse);

      // Set method specific responses
      EthResponse ethResponse = athenaResponse.getEthResponse(0);
      if (method.equals(EthMethodName.SEND_TX)
         || method.equals(EthMethodName.GET_STORAGE_AT)
         || method.equals(EthMethodName.CALL)
         || method.equals(EthMethodName.NEW_ACCOUNT)) {
         respObject.put("result",
                        APIHelper.binaryStringToHex(ethResponse.getData()));
      } else if (method.equals(EthMethodName.GET_TX_RECEIPT)) {
         JSONObject result = new JSONObject();
         result.put("status",
                    "0x" + Integer.toString(ethResponse.getStatus(), 16));
         result.put("transactionHash", txHash);
         if (ethResponse.hasContractAddress()) {
            result.put("contractAddress",
                       APIHelper.binaryStringToHex(ethResponse.getContractAddress()));
         } else {
            result.put("contractAddress", null);
         }
         respObject.put("result", result);
      } else if (method.equals(EthMethodName.NEW_FILTER) ||
                 method.equals(EthMethodName.NEW_BLOCK_FILTER) ||
                 method.equals(EthMethodName.NEW_PENDING_TRANSACTION_FILTER)) {
         ByteString filterBytes = ethResponse.getFilterResponse().getFilterId();
         respObject.put("result", APIHelper.binaryStringToHex(filterBytes));
      } else if (method.equals(EthMethodName.FILTER_CHANGES)) {
         JSONArray arr = new JSONArray();
         FilterResponse fresponse = ethResponse.getFilterResponse();
         for (ByteString hash : fresponse.getBlockHashesList()) {
            arr.add(APIHelper.binaryStringToHex(hash));
         }
         respObject.put("result", arr);
      } else if (method.equals(EthMethodName.UNINSTALL_FILTER)) {
         boolean success = ethResponse.getFilterResponse().getSuccess();
         respObject.put("result", success);
      } else {
         respObject = errorMessage("Unknown response type from athena",
                                   req.getEthRequest(0).getId());
      }

      processResponse(response,
                      respObject.toJSONString(),
                      HttpServletResponse.SC_OK,
                      log);
   }

   private void errorResponse(HttpServletResponse response, String message,
                              long id, Logger log) {
      processResponse(response,
                      errorMessage(message, id).toJSONString(),
                      HttpServletResponse.SC_OK,
                      log);
   }

   @SuppressWarnings("unchecked")
   private JSONObject errorMessage(String message, long id) {
      JSONObject responseJson = new JSONObject();
      responseJson.put("id", id);
      responseJson.put("jsonprc", jsonRpc);

      JSONObject error = new JSONObject();
      error.put("message", message);
      responseJson.put("error", error);

      return responseJson;
   }

   /**
    * Parses the Protocol Buffer response from Athena and converts it into JSON.
    *
    * @param athenaResponse
    *           Protocol Buffer object containing Athena's reponse
    * @return Response in JSON format
    */
   @SuppressWarnings("unchecked")
   @Override
   protected JSONObject parseToJSON(Athena.AthenaResponse athenaResponse) {
      // Extract the ethrpcexecute response from
      // the athena reponse envelope.
      EthResponse ethResponse = athenaResponse.getEthResponse(0);
      // Construct the response JSON object.
      JSONObject responseJson = new JSONObject();
      responseJson.put("id", ethResponse.getId());
      responseJson.put("jsonrpc", jsonRpc);
      return responseJson;
   }
}

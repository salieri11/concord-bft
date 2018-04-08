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
 * TODO : Handle the case of no/incorrect response from Athena
 */
package Servlets;

import com.google.protobuf.ByteString;
import com.vmware.athena.*;
import com.vmware.athena.Athena.ErrorResponse;
import com.vmware.athena.Athena.EthRequest.EthMethod;

import connections.AthenaConnectionPool;
import connections.IAthenaCommunication;
import connections.IAthenaConnection;

import com.vmware.athena.Athena.EthResponse;

import io.undertow.util.StatusCodes;
import java.io.IOException;
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

   private enum EthMethodName {
   SEND_TX, GET_TX_RECEIPT, GET_STORAGE_AT;
   }

   public EthRPC() throws ParseException {
      super();
      JSONParser p = new JSONParser();
      rpcList = (JSONArray) p.parse(_conf.getStringValue("EthRPCList"));
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
         processResponse(response, "error", StatusCodes.INTERNAL_SERVER_ERROR,
                  logger);
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
   protected void doPost(final HttpServletRequest request,
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
         String paramString = request.getReader().lines()
                  .collect(Collectors.joining(System.lineSeparator()));
         requestParams = (JSONObject) parser.parse(paramString);

         if (requestParams == null) {
            logger.error("Invalid request : Parameters should be in the "
                     + "request body and in a JSON object format");
            processResponse(response, "error", StatusCodes.BAD_REQUEST, logger);
            return;
         }
         logger.debug("Request Parameters: " + requestParams.toJSONString());

      } catch (ParseException e) {
         logger.error("Invalid request", e);
         processResponse(response, "error", StatusCodes.BAD_REQUEST, logger);
         return;
      }

      try {
         id = (Long) requestParams.get("id");
      } catch (NumberFormatException e) {
         logger.error("Invalid request parameter : id", e);
         processResponse(response, "error", StatusCodes.BAD_REQUEST, logger);
         return;
      }

      method = (String) requestParams.get("method");
      if (method == null || method.trim().length() < 1) {
         logger.error("Invalid request parameter : method");
         processResponse(response, "error", StatusCodes.BAD_REQUEST, logger);
         return;
      }

      params = (JSONArray) requestParams.get("params");
      if (params.size() < 1) {
         logger.error("Invalid request parameter : params");
         processResponse(response, "error", StatusCodes.BAD_REQUEST, logger);
         return;
      }

      Athena.EthRequest.Builder b = Athena.EthRequest.newBuilder();
      b.setId(id);

      // Set request fields as per the RPC
      try {
         if (method.equals(_conf.getStringValue("SendTransaction_Name"))) {
            rpc = EthMethodName.SEND_TX;
            b.setMethod(EthMethod.SEND_TX);
            JSONObject obj = (JSONObject) params.get(0);
            ByteString fromAddr = APIHelper
                     .hexStringToBinary((String) obj.get("from"));
            b.setAddrFrom(fromAddr);

            if (obj.containsKey("to")) {
               ByteString toAddr = APIHelper
                        .hexStringToBinary((String) obj.get("to"));
               b.setAddrTo(toAddr);
            }

            ByteString data = APIHelper
                     .hexStringToBinary((String) obj.get("data"));
            b.setData(data);
            if (obj.containsKey("value")) {
               ByteString value = APIHelper
                        .hexStringToBinary((String) obj.get("value"));
               b.setValue(value);
            }
         } else if (method
                  .equals(_conf.getStringValue("GetTransactionReceipt_Name"))) {
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
         } else {
            logger.error("Invalid method name");
            processResponse(response, "error", StatusCodes.BAD_REQUEST, logger);
            return;
         }
      } catch (Exception e) {
         logger.error("Invalid request parameter : params");
         processResponse(response, "error", StatusCodes.BAD_REQUEST, logger);
         return;
      }

      Athena.EthRequest athenaEthRequest = b.build();

      // Envelope the request object into an athena request object.
      final Athena.AthenaRequest athenarequestObj = Athena.AthenaRequest
               .newBuilder().addEthRequest(athenaEthRequest).build();

      processGet(rpc, txHash, athenarequestObj, response, logger);
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
            Athena.AthenaRequest req, HttpServletResponse response,
            Logger log) {
      JSONObject respObject = null;
      IAthenaConnection conn = null;
      Athena.AthenaResponse athenaResponse = null;
      try {
         conn = AthenaConnectionPool.getInstance().getConnection();
         boolean res = AthenaHelper.sendToAthena(req, conn, _conf);
         if (!res) {
            processResponse(response, "Communication error",
                     HttpServletResponse.SC_INTERNAL_SERVER_ERROR, log);
            return;
         }

         // receive response from Athena
         athenaResponse = AthenaHelper.receiveFromAthena(conn);
         if (athenaResponse == null) {
            processResponse(response, "Data error",
                     HttpServletResponse.SC_INTERNAL_SERVER_ERROR, log);
            return;
         }
      } catch (Exception e) {
         processResponse(response, "Internal error",
                  HttpServletResponse.SC_INTERNAL_SERVER_ERROR, log);
         return;
      } finally {
         AthenaConnectionPool.getInstance().putConnection(conn);
      }

      respObject = parseToJSON(athenaResponse);

      // If there is an error reported by Athena
      if (athenaResponse.getErrorResponseCount() > 0) {
         ErrorResponse errResponse = athenaResponse.getErrorResponse(0);
         if (errResponse.hasDescription()) {
            respObject.put("error", errResponse.getDescription());
         }
         processResponse(response, respObject.toJSONString(),
                  HttpServletResponse.SC_OK, log);
         return;
      }

      // Set method specific responses
      EthResponse ethResponse = athenaResponse.getEthResponse(0);
      if (method.equals(EthMethodName.SEND_TX)
               || method.equals(EthMethodName.GET_STORAGE_AT)) {
         respObject.put("result",
                  APIHelper.binaryStringToHex(ethResponse.getData()));
      } else if (method.equals(EthMethodName.GET_TX_RECEIPT)) {
         JSONObject result = new JSONObject();
         result.put("status",
                  "0x" + Integer.toString(ethResponse.getStatus(), 16));
         result.put("transactionHash", txHash);
         if (ethResponse.hasContractAddress()) {
            result.put("contractAddress", APIHelper
                     .binaryStringToHex(ethResponse.getContractAddress()));
         } else {
            result.put("contractAddress", null);
         }
         respObject.put("result", result);
      }

      String json = respObject == null ? null : respObject.toJSONString();
      processResponse(response, json,
               json == null ? HttpServletResponse.SC_INTERNAL_SERVER_ERROR
                        : HttpServletResponse.SC_OK,
               log);
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
      responseJson.put("jsonrpc", _conf.getStringValue("JSONRPC"));
      return responseJson;
   }
}

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
   private JSONObject rpcModules;
   private String jsonRpc;
   private String clientVersion;
   private String coinbase;
   private boolean isMining;

   public EthDispatcher() throws ParseException {
      super();
      JSONParser p = new JSONParser();
      try {
         rpcList = (JSONArray) p.parse(_conf.getStringValue("EthRPCList"));
         rpcModules
            = (JSONObject) (((JSONArray) p.parse(_conf.getStringValue("RPCModules"))).get(0));
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
            processResponse(response,
                            errorMessage("Unable to parse request", 0),
                            HttpServletResponse.SC_OK,
                            logger);
            return;
         }
         logger.debug("Request Parameters: " + requestParams.toJSONString());

      } catch (ParseException e) {
         logger.error("Invalid request", e);
         processResponse(response,
                         errorMessage("Unable to parse request", 0),
                         HttpServletResponse.SC_OK,
                         logger);
         return;
      }

      try {
         id = (Long) requestParams.get("id");
      } catch (NumberFormatException e) {
         logger.error("Invalid request parameter : id", e);
         processResponse(response,
                         errorMessage("'id' must be an integer", 0),
                         HttpServletResponse.SC_OK,
                         logger);
         return;
      }

      method = (String) requestParams.get("method");
      if (method == null || method.trim().length() < 1) {
         logger.error("Invalid request parameter : method");
         processResponse(response,
                         errorMessage("Invalid 'method' parameter", id),
                         HttpServletResponse.SC_OK,
                         logger);
         return;
      }

      try {
         params = (JSONArray) requestParams.get("params");
      } catch (ClassCastException cse) {
         logger.error("Invalid request parameter : params");
         processResponse(response,
                         errorMessage("'params' must be an array", id),
                         HttpServletResponse.SC_OK,
                         logger);
         return;
      }

      // Dispatch requests to the corresponding handlers
      try {
         dispatch(response, id, method, params, txHash);
      } catch (Exception e) {
         logger.error("Invalid " + method + " request", e);
         processResponse(response,
                         errorMessage(e.getMessage(), id),
                         HttpServletResponse.SC_OK,
                         logger);
         return;
      }
   }

   @SuppressWarnings("unchecked")
   private void dispatch(final HttpServletResponse response, Long id,
                         String method, JSONArray params,
                         String txHash) throws Exception {
      Athena.AthenaRequest athenaRequestObj = null;
      IEthRPC handler = null;
      boolean isLocal = false;
      String responseString = null;
      Object localData = null;

      // Dispatch to appropriate handlers

      if (method.equals(_conf.getStringValue("SendTransaction_Name"))
         || method.equals(_conf.getStringValue("Call_Name"))) {
         handler = new EthSendTxHandler();
      } else if (method.equals(_conf.getStringValue("NewAccount_Name"))) {
         handler = new EthNewAccountHandler();
      } else if (method.equals(_conf.getStringValue("GetTransactionReceipt_Name"))) {
         handler = new EthGetTxReceiptHandler();
         txHash = (String) params.get(0);
      } else if (method.equals(_conf.getStringValue("GetStorageAt_Name"))) {
         handler = new EthGetStorageAtHandler();
      } else if (method.equals(_conf.getStringValue("GetCode_Name"))) {
         handler = new EthGetCodeHandler();
      } else if (method.equals(_conf.getStringValue("NewFilter_Name"))
         || method.equals(_conf.getStringValue("NewBlockFilter_Name"))
         || method.equals(_conf.getStringValue("NewPendingTransactionFilter_Name"))
         || method.equals(_conf.getStringValue("FilterChange_Name"))
         || method.equals(_conf.getStringValue("UninstallFilter_Name"))) {
         handler = new EthFilterHandler();
      }
      // Local responses
      else if (method.equals(_conf.getStringValue("Web3SHA3_Name"))) {
         // Request should contain just one param value
         if (params.size() != 1) {
            logger.error("Invalid request parameter : params");
            processResponse(response,
                            errorMessage("'params' must contain only one element",
                                         id),
                            HttpServletResponse.SC_OK,
                            logger);
            return;
         }
         isLocal = true;
         try {
            localData = APIHelper.getKeccak256Hash((String) params.get(0));
         } catch (Exception e) {
            logger.error("Error in calculating Keccak hash");
            processResponse(response,
                            errorMessage("Invalid param", id),
                            HttpServletResponse.SC_OK,
                            logger);
            return;
         }
         handler = new EthLocalResponseHandler();
      } else {
         isLocal = true;
         if (method.equals(_conf.getStringValue("RPCModules_Name"))) {
            handler = new EthLocalResponseHandler();
            localData = rpcModules;
         } else if (method.equals(_conf.getStringValue("Coinbase_Name"))) {
            handler = new EthLocalResponseHandler();
            localData = coinbase;
         } else if (method.equals(_conf.getStringValue("ClientVersion_Name"))) {
            handler = new EthLocalResponseHandler();
            localData = clientVersion;
         } else if (method.equals(_conf.getStringValue("Mining_Name"))) {
            handler = new EthLocalResponseHandler();
            localData = isMining;
         } else if (method.equals(_conf.getStringValue("NetVersion_Name"))) {
            if (!netVersionSet) {
               // The act of creating a connection retrieves info about athena.
               IAthenaConnection conn = null;
               try {
                  conn = AthenaConnectionPool.getInstance().getConnection();
               } catch (IllegalStateException | InterruptedException e) {
                  e.printStackTrace();
               }
               if (conn == null) {
                  String failureMsg = "Unable to connect to athena.";
                  logger.error(failureMsg);
                  processResponse(response,
                                  errorMessage(failureMsg, id),
                                  HttpServletResponse.SC_OK,
                                  logger);
                  return;
               } else {
                  netVersionSet = true;
               }
            }
            handler = new EthLocalResponseHandler();
            localData = netVersion;
         } else if (method.equals(_conf.getStringValue("Accounts_Name"))) {
            JSONArray usersJsonArr = new JSONArray();
            String usersStr = _conf.getStringValue("USERS");
            if (usersStr != null && !usersStr.trim().isEmpty()) {
               String[] usersArr = usersStr.split(",");

               for (int i = 0; i < usersArr.length; i++) {
                  usersJsonArr.add(usersArr[i]);
               }
            }
            handler = new EthLocalResponseHandler();
            localData = usersJsonArr;
         } else {
            logger.error("Invalid method name");
            processResponse(response,
                            errorMessage("Unknown method '" + method + "'", id),
                            HttpServletResponse.SC_OK,
                            logger);
            return;
         }
      }

      AthenaResponse athenaResponse = null;
      if (!isLocal) {
         try {
            athenaRequestObj = handler.handleRequest(id, method, params);
         } catch (Exception e) {
            logger.error("Invalid " + method + " request", e);
            processResponse(response,
                            errorMessage(e.getMessage(), id),
                            HttpServletResponse.SC_OK,
                            logger);
            return;
         }
         athenaResponse
            = communicateWithAthena(athenaRequestObj, response, logger);
         // If there is an error reported by Athena
         if (athenaResponse.getErrorResponseCount() > 0) {
            ErrorResponse errResponse = athenaResponse.getErrorResponse(0);
            String description = "Error received from athena";
            if (errResponse.hasDescription()) {
               description = errResponse.getDescription();
            }
            processResponse(response,
                            errorMessage(description, id),
                            HttpServletResponse.SC_OK,
                            logger);
            return;
         }
         responseString = handler.buildResponse(athenaResponse, txHash, method);
      } else {
         responseString = handler.buildLocalResponse(localData, id);
      }

      processResponse(response,
                      responseString,
                      HttpServletResponse.SC_OK,
                      logger);
      return;
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
                                         req.getEthRequest(0).getId()),
                            HttpServletResponse.SC_OK,
                            log);
            return null;
         }

         boolean res = AthenaHelper.sendToAthena(req, conn, _conf);
         if (!res) {
            processResponse(response,
                            errorMessage("Error communicating with athena",
                                         req.getEthRequest(0).getId()),
                            HttpServletResponse.SC_OK,
                            log);
            return null;
         }

         // receive response from Athena
         athenaResponse = AthenaHelper.receiveFromAthena(conn);
         if (athenaResponse == null) {
            processResponse(response,
                            errorMessage("Error reading response from athena",
                                         req.getEthRequest(0).getId()),
                            HttpServletResponse.SC_OK,
                            log);
            return null;
         }
      } catch (Exception e) {
         logger.error("General exception communicating with athena: ", e);
         processResponse(response,
                         errorMessage("Error communicating with athena",
                                      req.getEthRequest(0).getId()),
                         HttpServletResponse.SC_OK,
                         log);
         return null;
      } finally {
         AthenaConnectionPool.getInstance().putConnection(conn);
      }

      return athenaResponse;
   }

   @SuppressWarnings("unchecked")
   private String errorMessage(String message, long id) {
      JSONObject responseJson = new JSONObject();
      responseJson.put("id", id);
      responseJson.put("jsonprc", jsonRpc);

      JSONObject error = new JSONObject();
      error.put("message", message);
      responseJson.put("error", error);

      return responseJson.toJSONString();
   }

   @Override
   protected JSONAware parseToJSON(AthenaResponse athenaResponse) {
      return null;
   }
}

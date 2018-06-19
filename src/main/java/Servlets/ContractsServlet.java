package Servlets;

import java.io.IOException;
import java.io.UnsupportedEncodingException;
import java.net.URLDecoder;
import java.net.URLEncoder;
import java.nio.charset.StandardCharsets;
import java.util.List;
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

import contracts.*;
import contracts.Compiler;

/**
 * <p>
 * Copyright 2018 VMware, all rights reserved.
 * </p>
 *
 * A servlet which handles all contract management queries sent to
 * `api/athena/contracts/*` URI.
 */
public class ContractsServlet extends BaseServlet {

   private static final long serialVersionUID = 1L;
   private final static Logger logger
      = Logger.getLogger(ContractsServlet.class);
   private final String jsonRpc = _conf.getStringValue("JSONRPC");
   private final String contractEndpoint
      = _conf.getStringValue("Contracts_Endpoint");
   private ContractRegistryManager registryManager;

   public ContractsServlet() {
      try {
         registryManager = ContractRegistryManager.getInstance();
      } catch (Exception e) {
         logger.fatal("Unable to instantiate ContractRegistryManager", e);
         registryManager = null;
      }
   }

   /**
    * Encodes the given string as per URL encoding rules
    * 
    * @param str
    * @return
    */
   private String urlEncode(String str) {
      String encodedStr;
      try {
         encodedStr = URLEncoder.encode(str, StandardCharsets.UTF_8.name());
      } catch (UnsupportedEncodingException e) {
         encodedStr = str;
         logger.warn("URL encoding exception", e);
      }
      return encodedStr;
   }

   /**
    * Builds a JSON which is sent as a response object for `GET
    * api/athena/contracts` queries
    * 
    * @return JSONObject which should be returned to client
    */
   private JSONArray buildContractsJSON() {
      JSONArray cArray = new JSONArray();
      List<BriefContractInfo> cInfoList
         = registryManager.getAllBriefContractInfo();

      for (BriefContractInfo cinfo : cInfoList) {
         JSONObject contract = new JSONObject();
         contract.put("contract_id", cinfo.getContractId());
         contract.put("owner", cinfo.getOwnerAddress());
         contract.put("url",
                      contractEndpoint + "/"
                         + urlEncode(cinfo.getContractId()));
         cArray.add(contract);
      }
      return cArray;
   }

   /**
    * Processes the `GET api/athena/contracts` request and puts the generated
    * result inside @responseObject.
    * 
    * @param responseObject
    *           The JSONObject which will contain the result generated during
    *           processing. This is an *OUT* parameter i.e this method will put
    *           the result inside this object. The result will be either a
    *           JSONObject or a JSONArray and will be stored with "result" as
    *           the key. For e.g responseObject.put("result", resultJSONObject);
    * @return The Http status code of response.
    */
   private int handleGetContracts(JSONObject responseObject) {
      responseObject.put("result", buildContractsJSON());
      return HttpServletResponse.SC_OK;
   }

   /**
    * Builds a JSONObject from the given List of BriefVersionInfo objects. This
    * JSONObject is generated as per the response format of `GET
    * api/athena/contracts/{contract_id}` request type.
    *
    * @param briefVersionInfoList
    * @return Returns the json object containing information about all versions
    *         present in the list.
    */
   private JSONObject
           buildContractJSON(List<BriefVersionInfo> briefVersionInfoList) {
      // construct response json from contract object
      boolean contractInfoAdded = false;
      JSONObject contract = new JSONObject();
      JSONArray versions = new JSONArray();
      for (BriefVersionInfo briefVersionInfo : briefVersionInfoList) {
         if (!contractInfoAdded) {
            contract.put("contract_id", briefVersionInfo.getContractId());
            contract.put("owner", briefVersionInfo.getOwnerAddress());
            contractInfoAdded = true;
         }
         JSONObject version = new JSONObject();
         JSONObject metadata = null;
         try {
            JSONParser parser = new JSONParser();
            metadata
               = (JSONObject) parser.parse(briefVersionInfo.getMetaData());
         } catch (ParseException e) {
            logger.warn("Metadata parsing failed", e);
         }
         version.put("version", briefVersionInfo.getVersionName());
         version.put("address", briefVersionInfo.getAddress());
         version.put("metadata", metadata);
         // TODO : is there a better way to do it?
         version.put("url",
                     contractEndpoint + "/"
                        + urlEncode(briefVersionInfo.getContractId())
                        + "/versions/"
                        + urlEncode(briefVersionInfo.getVersionName()));
         versions.add(version);
      }
      contract.put("versions", versions);
      return contract;
   }

   /**
    * Handles the `GET api/athena/contracts/{contract_id}` request.
    * 
    * @param contractId
    *           The `contract_id` variable present in the request URI
    * @param responseObject
    *           The JSONObject in which the result of this request should be
    *           stored. The result will be either a JSONObject or a JSONArray
    *           and will be stored with "result" as the key. For e.g
    *           responseObject.put("result", resultJSONObject);
    * @return The Http status code of response
    */
   private int handleGetContract(String contractId, JSONObject responseObject) {
      int respStatus = HttpServletResponse.SC_OK;
      if (registryManager.hasContract(contractId)) {
         responseObject.put("result",
                            buildContractJSON(registryManager.getAllBriefVersionInfo(contractId)));
      } else {
         JSONObject error = new JSONObject();
         respStatus = HttpServletResponse.SC_NOT_FOUND;
         error.put("message", "No contract found with id: " + contractId);
         responseObject.put("error", error);
      }
      return respStatus;
   }

   /**
    * Builds a JSONObject representing the given version of the given contract
    * object. The JSONObject is generated to follow the response format of `GET
    * api/athena/contracts/{contract_id}/versions/{version}` request.
    * 
    * @param versionInfo
    * @return The generated JSON with all details of the specific version
    */
   private JSONObject buildVersionJSON(FullVersionInfo versionInfo) {
      JSONObject versionJSON = new JSONObject();
      JSONParser parser = new JSONParser();
      JSONObject metadata = null;
      try {
         metadata = (JSONObject) parser.parse(versionInfo.getMetaData());
      } catch (ParseException e) {
         logger.warn("Metadata parsing failure: ", e);
      }
      versionJSON.put("contract_id", versionInfo.getContractId());
      versionJSON.put("owner", versionInfo.getOwnerAddress());
      versionJSON.put("version", versionInfo.getVersionName());
      versionJSON.put("metadata", metadata);
      versionJSON.put("address", versionInfo.getAddress());
      versionJSON.put("bytecode", versionInfo.getByteCode());
      versionJSON.put("sourcecode", versionInfo.getSourceCode());
      return versionJSON;
   }

   /**
    * Handles the `GET api/athena/contracts/{contract_id}/versions/{version }`
    * request.
    * 
    * @param contractId
    *           The value of `{contract_id}` from URI
    * @param contractVersion
    *           The value of `{version}` from URI
    * @param responseObject
    *           The response JSONObject inside which the generated response will
    *           be stored.The result will be either a JSONObject or a JSONArray
    *           and will be stored with "result" as the key. For e.g
    *           responseObject.put("result", resultJSONObject);
    * @return The http status code of response.
    */
   private int handleGetVersion(String contractId, String contractVersion,
                                JSONObject responseObject) {
      int respStatus = HttpServletResponse.SC_OK;
      try {
         FullVersionInfo fvInfo
            = registryManager.getContractVersion(contractId, contractVersion);
         responseObject.put("result", buildVersionJSON(fvInfo));
      } catch (ContractRetrievalException e) {
         JSONObject error = new JSONObject();
         respStatus = HttpServletResponse.SC_NOT_FOUND;
         error.put("message",
                   "No contract found with id: " + contractId + " and version: "
                      + contractVersion);
         responseObject.put("error", error);
      }
      return respStatus;
   }

   /**
    * ContractsServlet handles 4 types of URIs as of now, these are:
    *
    * GET `api/athena/contracts`
    *
    * GET `api/athena/contracts/{contract-id}`
    *
    * GET `api/athena/contracts/{contract-id}/versions/{version-id}`
    *
    * POST `api/athena/contracts`
    *
    * However, due to current architecture of helen we can not use undertow's
    * RoutingHandler class to cleanly handle these REST requests to proper
    * handlers, hence we have a single ContractsServlet for any request of URI
    * type `api/athena/contracts/*` and then this dispatch method properly
    * forwards it to correct method.
    * 
    * @param request
    * @param response
    */
   protected void dispatch(final HttpServletRequest request,
                           final HttpServletResponse response) {
      String uri = request.getRequestURI();
      try {
         uri = URLDecoder.decode(uri, StandardCharsets.UTF_8.name());
      } catch (UnsupportedEncodingException e) {
         logger.warn("URI decoding failed: ", e);
      }
      logger.debug("Decoded URI: " + uri);
      int respStatus = HttpServletResponse.SC_OK;
      JSONObject responseJSON = new JSONObject();

      // TODO: this nullcheck is fragile. We need to redesign this database
      // service classes so that other servlets work even if database is
      // unavailable
      if (registryManager == null) {
         // throw error
         JSONObject error = new JSONObject();
         error.put("message", "Service unavailable.");
         responseJSON.put("error", error);
         respStatus = HttpServletResponse.SC_SERVICE_UNAVAILABLE;
      } else if (request.getMethod().equalsIgnoreCase("GET")) {
         // If uri has trailing backslash remove it
         if (uri.charAt(uri.length() - 1) == '/') {
            uri = uri.substring(0, uri.length() - 1);
         }
         // remove contract endpoint common prefix
         uri = uri.replace(contractEndpoint, "");
         String tokens[] = uri.split("/");
         if (tokens.length == 1) {
            // GET `api/athena/contracts`
            respStatus = handleGetContracts(responseJSON);
         } else if (tokens.length == 2) {
            // GET `api/athena/contracts/{contract_id}`
            respStatus = handleGetContract(tokens[1], responseJSON);
         } else if (tokens.length == 4) {
            // GET `api/athena/contracts/{contract-id}/versions/{version-id}`
            respStatus = handleGetVersion(tokens[1], tokens[3], responseJSON);
         }
      } else {
         // throw error
         JSONObject error = new JSONObject();
         error.put("message", "Requested Method not allowed!");
         responseJSON.put("error", error);
         respStatus = HttpServletResponse.SC_METHOD_NOT_ALLOWED;
      }
      processResponse(response,
                      responseJSON.toJSONString(),
                      respStatus,
                      logger);
   }

   /**
    * Handles the GET request for `/api/athena/contracts/<name>` API. GET
    * request returns the solidity code, bytecode and metadata information of
    * that contract
    * 
    * @param request
    * @param response
    * @throws IOException
    */
   @Override
   protected void doGet(final HttpServletRequest request,
                        final HttpServletResponse response) throws IOException {
      dispatch(request, response);
   }

   /**
    * Builds a JSONObject which represents a request of type
    * `eth_sendTransaction` (Ethereum send transaction)
    * 
    * @param from
    *           Address from which new contract creation request was sent
    * @param id
    *           Request id
    * @param byteCode
    *           bytecode of compiled contract
    * @return JSON object of ethereum request
    */
   private JSONObject buildEthSendTxRequest(String from, long id,
                                            String byteCode) {
      // Now make a eth_sendTransaction request json object
      JSONObject ethRequest = new JSONObject();
      JSONArray paramsArray = new JSONArray();
      JSONObject transaction = new JSONObject();
      ethRequest.put("id", id);
      ethRequest.put("jsonrpc", jsonRpc);
      ethRequest.put("method", "eth_sendTransaction");
      transaction.put("from", from);
      transaction.put("data", byteCode);
      paramsArray.add(transaction);
      ethRequest.put("params", paramsArray);
      return ethRequest;
   }

   /**
    * Builds an Ethereum Get Transaction Receipt (eth_getTransactionReceipt)
    * request JSON object.
    * 
    * @param id
    *           The request id
    * @param transactionHash
    *           The hash of transction whose receipt is needed
    * @return the JSON request object
    */
   private JSONObject buildEthTxReceiptRequest(long id,
                                               String transactionHash) {
      // Now make a eth_sendTransaction request json object
      JSONObject ethRequest = new JSONObject();
      JSONArray paramsArray = new JSONArray();
      ethRequest.put("id", id);
      ethRequest.put("jsonrpc", jsonRpc);
      ethRequest.put("method", "eth_getTransactionReceipt");
      paramsArray.add(transactionHash);
      ethRequest.put("params", paramsArray);
      return ethRequest;
   }

   /**
    * Extracts the contracts address from the response of the
    * `eth_getTransactionReceipt` request.
    * 
    * @param jsonStr
    *           The string representing JSONObject of the response of
    *           `eth_getTransactionReceipt` request.
    * @return The value of key "result" from given JSON string which ideally
    *         represents the address at which contract is deployed.
    * @throws ParseException
    */
   private String extractContractAddress(String jsonStr) throws ParseException {
      JSONParser parser = new JSONParser();
      JSONObject json = (JSONObject) parser.parse(jsonStr);
      JSONObject result = (JSONObject) json.get("result");
      String address = (String) result.get("contractAddress");
      return address;
   }

   /**
    * Deploys the given set of compiled contracts to athena. A common way of
    * deploying a new contract to athena is by sending a `eth_sendTransaction`
    * request to Athena with contract code in that request. Here, we utilize the
    * same functionality (since it is already implemented in helen). We make a
    * `eth_sendTransaction` request for every contract and send them to athena
    * in a loop. After every request we call `eth_getTransactionReceipt` to
    * either get the address at which contract was deployed or the error in case
    * of failure.
    * 
    * @param contractId
    *           The unique contract_id provided by the user
    * @param contractVersion
    *           The version number for this contract provided by the user.
    * @param from
    *           Address of the account which initiated this request.
    * @param requestId
    *           ID of the RPC request
    * @param result
    *           Compiler.Result object which contains all compilation outputs
    *           generated by compiling given solidity file
    * @param solidityCode
    *           The actual solidity code which was compiled
    * @return The JSONArray containing results of deployment of each contract
    *         present in solidity code.
    * @throws Exception
    */
   private JSONArray deployContracts(String contractId, String contractVersion,
                                     String from, long requestId,
                                     Compiler.Result result,
                                     String solidityCode) throws Exception {
      JSONArray resultArray = new JSONArray();

      // If we have multiple contracts in the solidity file then things get
      // difficult. For example, using the unique `id` provided by the
      // user with multiple contracts is an issue. Second is what if athena
      // gives error while deploying one of the contracts, should be rollback
      // (how?) or should we just return with half of the contracts deployed?
      // Currently we throw error if file has multiple contracts
      // TODO: support multiple contracts in the file.

      // We have already made sure in handlePost method that this
      // result does not have more than two contracts, hence a for loop is
      // okay to use. Plus later on we might support multiple contracs in which
      // case we will use a loop anyway.
      // Note: contractName and contractId are two different things. contractId
      // is the unique id provided to the contract by user while contractName
      // is the name of contract present in solidity file.
      for (String contractName : result.getByteCodeMap().keySet()) {
         // Since EthDispatcher already has the code of handling ethereum
         // requests we just build a JSON object representing an ethereum
         // request (as if it was received like a normal ethereum JSON RPC)
         // and forward it to EthDispatcher.
         JSONObject sendTxrequest
            = buildEthSendTxRequest(from,
                                    requestId,
                                    result.getByteCodeMap().get(contractName));
         String responseString = new EthDispatcher().dispatch(sendTxrequest);
         JSONObject ethResponse
            = (JSONObject) new JSONParser().parse(responseString);

         // Build response object for this particular deployment
         JSONObject deploymentResult = new JSONObject();
         deploymentResult.put("contract_id", contractId);
         deploymentResult.put("version", contractVersion);

         if (ethResponse.containsKey("result")) {
            String transactionHash = (String) ethResponse.get("result");
            // Now call eth_getTransactionReceipt API to get the address
            // of deployed contract
            JSONObject txReceiptRequest
               = buildEthTxReceiptRequest(requestId, transactionHash);
            String txReceipt = new EthDispatcher().dispatch(txReceiptRequest);
            logger.info("New contract deployed at: "
               + extractContractAddress(txReceipt));

            boolean success
               = registryManager.addNewContractVersion(contractId,
                                                       from,
                                                       contractVersion,
                                                       extractContractAddress(txReceipt),
                                                       result.getMetadataMap()
                                                             .get(contractName),
                                                       result.getByteCodeMap()
                                                             .get(contractName),
                                                       solidityCode);

            if (success) {
               deploymentResult.put("url",
                                    contractEndpoint + "/"
                                       + urlEncode(contractId) + "/versions/"
                                       + urlEncode(contractVersion));
               deploymentResult.put("error", null);
            } else {
               deploymentResult.put("url", null);
               deploymentResult.put("error", "Internal error.");
            }
         } else {
            // If transactionHash == null then there was error from
            // athena, forward it to client as it is
            deploymentResult.put("url", null);
            deploymentResult.put("error", ethResponse.get("error"));
         }
         resultArray.add(deploymentResult);
      }
      return resultArray;
   }

   private boolean isSameAddress(String address1, String address2) {
      if (!address1.startsWith("0x")) {
         address1 = "0x" + address1;
      }
      if (!address2.startsWith("0x")) {
         address2 = "0x" + address2;
      }
      return address1.equals(address2);
   }

   /**
    * Handles the POST request for `/api/athena/contracts` API. A POST request
    * expects a solidity code as a part of the POST body. The POST body must be
    * a JSON having a key "params" and value being a strings of solidity
    * contract code. Every single of these contract codes are compiled and
    * deployed to athena
    * 
    * @param request
    *           HttpRequest object
    * @param response
    *           HttpResponse object
    * @param responseJSON
    *           The JSONObject which should be filled with the results of
    *           processing. The results of processing will be inserted into this
    *           json object under "result" (in case of success) or "error" (in
    *           case of failure) keys.
    * @return
    */
   private int handlePost(final HttpServletRequest request,
                          final HttpServletResponse response,
                          JSONObject responseJSON) {
      long id = -1;
      int respStatus;
      try {
         String paramString
            = request.getReader()
                     .lines()
                     .collect(Collectors.joining(System.lineSeparator()));
         logger.debug(paramString);
         JSONParser parser = new JSONParser();
         JSONObject requestObject = (JSONObject) parser.parse(paramString);
         // First extract some fields from requestParams
         id = (long) requestObject.get("id");
         String from = (String) requestObject.get("from");
         String contractId = (String) requestObject.get("contract_id");
         String contractVersion = (String) requestObject.get("version");
         String solidityCode = (String) requestObject.get("sourcecode");

         // Check if contract with same id already exists, if yes
         // then version number must be different
         if (registryManager.hasContractVersion(contractId, contractVersion)) {
            respStatus = HttpServletResponse.SC_CONFLICT;
            APIHelper.fillErrorMessage(responseJSON,
                                       "contract with "
                                          + "same name and version already exists",
                                       id,
                                       jsonRpc);

         } else if (registryManager.hasContract(contractId)
            && !isSameAddress(registryManager.getBriefContractInfo(contractId)
                                             .getOwnerAddress(),
                              (from))) {
            // It is a new version of
            // existing contract but from address doesn't match
            respStatus = HttpServletResponse.SC_FORBIDDEN;
            APIHelper.fillErrorMessage(responseJSON,
                                       "Only original "
                                          + "owner can deploy the new version of a contract",
                                       id,
                                       jsonRpc);
         } else {
            // Compile the given solidity code
            Compiler.Result result = Compiler.compile(solidityCode);
            logger.debug(result);
            if (result.isSuccess() && result.getByteCodeMap().size() == 1) {
               JSONArray resultArray = deployContracts(contractId,
                                                       contractVersion,
                                                       from,
                                                       id,
                                                       result,
                                                       solidityCode);
               respStatus = HttpServletResponse.SC_OK;
               responseJSON.put("result", resultArray);
               responseJSON.put("id", id);
               responseJSON.put("jsonrpc", jsonRpc);
            } else if (result.isSuccess()
               && result.getByteCodeMap().size() != 1) {
               respStatus = HttpServletResponse.SC_BAD_REQUEST;
               APIHelper.fillErrorMessage(responseJSON,
                                          "Uploaded file must have exactly one contract",
                                          id,
                                          jsonRpc);
            } else {
               respStatus = HttpServletResponse.SC_BAD_REQUEST;
               APIHelper.fillErrorMessage(responseJSON,
                                          "Compilation failure:\n"
                                             + result.getStderr(),
                                          id,
                                          jsonRpc);
            }
         }
      } catch (ParseException pe) {
         logger.warn("Exception while parsing request JSON", pe);
         respStatus = HttpServletResponse.SC_BAD_REQUEST;
         APIHelper.fillErrorMessage(responseJSON,
                                    "Unable to parse " + "request",
                                    id,
                                    jsonRpc);
      } catch (Exception e) {
         logger.warn("Exception in request processing", e);
         respStatus = HttpServletResponse.SC_INTERNAL_SERVER_ERROR;
         APIHelper.fillErrorMessage(responseJSON, e.getMessage(), id, jsonRpc);
      }
      logger.debug("Response: " + responseJSON.toJSONString());
      return respStatus;
   }

   /**
    * Accepts a POST requests and forwards it to the dispatcher.
    * 
    * @param request
    * @param response
    * @throws IOException
    */
   protected void
             doPost(final HttpServletRequest request,
                    final HttpServletResponse response) throws IOException {
      int respStatus;
      JSONObject responseJSON = new JSONObject();
      if (registryManager == null) {
         respStatus = HttpServletResponse.SC_SERVICE_UNAVAILABLE;
         // throw error
         JSONObject error = new JSONObject();
         error.put("message", "Service unavailable.");
         responseJSON.put("error", error);
      } else {
         respStatus = handlePost(request, response, responseJSON);
      }
      processResponse(response,
                      responseJSON.toJSONString(),
                      respStatus,
                      logger);
   }

   /**
    * This method is not required in this servlet since we never directly
    * contact athena here. We only forward request to other servlets which talk
    * with athena.
    *
    * @param athenaResponse
    *           Protocol Buffer object containing Athena's reponse
    * @return Response in JSON format
    */
   @SuppressWarnings("unchecked")
   @Override
   protected JSONAware
             parseToJSON(Athena.AthenaResponse athenaResponse) throws UnsupportedOperationException {
      throw new UnsupportedOperationException("parseToJSON is not supported "
         + "in " + "ContractsServlet");
   }
}

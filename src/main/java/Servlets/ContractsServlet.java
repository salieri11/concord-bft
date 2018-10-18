package Servlets;

import static Servlets.APIHelper.errorJSON;

import java.io.UnsupportedEncodingException;
import java.net.URLEncoder;
import java.nio.charset.StandardCharsets;
import java.util.List;
import java.util.Random;

import javax.annotation.PostConstruct;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.json.simple.JSONArray;
import org.json.simple.JSONAware;
import org.json.simple.JSONObject;
import org.json.simple.parser.JSONParser;
import org.json.simple.parser.ParseException;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.stereotype.Controller;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestMethod;

import com.vmware.athena.Athena;

import services.contracts.*;
import services.contracts.Compiler;

/**
 * <p>
 * Copyright 2018 VMware, all rights reserved.
 * </p>
 *
 * A servlet which handles all contract management queries sent to
 * `api/athena/contracts/*` URI.
 */
@Controller
public class ContractsServlet extends BaseServlet {

   private static final long serialVersionUID = 1L;
   private final static Logger logger
      = LogManager.getLogger(ContractsServlet.class);
   private final String jsonRpc = _conf.getStringValue("JSONRPC");
   private final String contractEndpoint
      = _conf.getStringValue("Contracts_Endpoint");
   private ContractRegistryManager registryManager;
   private Random random = new Random();

   // TODO: Since, Database related things for contracts is still handled by
   // old code it can't be autowired during setup of this Bean. Hence, we
   // call this method once bean construction is complete (@PostConstruct)
   // and do initialization. Once we start using JPA repositories for contracts
   // we can remove this and let spring AutoWire ContractRegistryManager
   @PostConstruct
   public void init() {
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
    * @return the RESTResult object which contains response of this request.
    */
   @RequestMapping(method = RequestMethod.GET, path = "/api/athena/contracts")
   public ResponseEntity<JSONAware> handleGetContracts() {

      // TODO: This check is not a proper way, find a better approach
      if (registryManager == null) {
         return new ResponseEntity<>(errorJSON("Service unavailable."),
                                     standardHeaders,
                                     HttpStatus.SERVICE_UNAVAILABLE);
      }

      return new ResponseEntity<>(buildContractsJSON(),
                                  standardHeaders,
                                  HttpStatus.OK);
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
    *
    * @return The RESTResult object containing result of this request
    */
   @RequestMapping(method = RequestMethod.GET,
                   path = "/api/athena/contracts/{contract_id}")
   public ResponseEntity<JSONAware>
          handleGetContract(@PathVariable("contract_id") String contractId) {

      // TODO: This check is not a proper way, find a better approach
      if (registryManager == null) {
         return new ResponseEntity<>(errorJSON("Service unavailable."),
                                     standardHeaders,
                                     HttpStatus.SERVICE_UNAVAILABLE);
      }

      if (registryManager.hasContract(contractId)) {
         return new ResponseEntity<>(buildContractJSON(registryManager.getAllBriefVersionInfo(contractId)),
                                     standardHeaders,
                                     HttpStatus.OK);
      } else {
         return new ResponseEntity<>(errorJSON("No contract found with id: "
            + contractId), standardHeaders, HttpStatus.NOT_FOUND);
      }
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
         metadata = new JSONObject();
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
    * @return The RESTResult object containing result of this request
    */
   @RequestMapping(method = RequestMethod.GET,
                   path = "/api/athena/contracts/{contract_id}/versions/{version_id}")
   public ResponseEntity<JSONAware>
          handleGetVersion(@PathVariable("contract_id") String contractId,
                           @PathVariable("version_id") String contractVersion) {

      // TODO: This check is not a proper way, find a better approach
      if (registryManager == null) {
         return new ResponseEntity<>(errorJSON("Service unavailable."),
                                     standardHeaders,
                                     HttpStatus.SERVICE_UNAVAILABLE);
      }

      try {
         FullVersionInfo fvInfo
            = registryManager.getContractVersion(contractId, contractVersion);
         return new ResponseEntity<>(buildVersionJSON(fvInfo),
                                     standardHeaders,
                                     HttpStatus.OK);
      } catch (ContractRetrievalException e) {
         return new ResponseEntity<>(errorJSON("No contract found with id: "
            + contractId + " and version: " + contractVersion),
                                     standardHeaders,
                                     HttpStatus.NOT_FOUND);
      }
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
      ethRequest.put("isInternalContract", true);
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

   @RequestMapping(method = RequestMethod.PUT,
           path = "/api/athena/contracts/{contract_id}/versions/{version_id}")
   public ResponseEntity<JSONAware>
   handleUpdateVersion(@RequestBody String paramString, @PathVariable("contract_id") String existingContractId,
                    @PathVariable("version_id") String existingVersionName) {

      // TODO: This check is not a proper way, find a better approach
      if (registryManager == null) {
         return new ResponseEntity<>(errorJSON("Service unavailable."),
                 standardHeaders,
                 HttpStatus.SERVICE_UNAVAILABLE);
      }

      ResponseEntity<JSONAware> responseEntity;

      try {

//         TODO: Compile the provided source code and update for the given contract with parameters
         JSONParser parser = new JSONParser();
         JSONObject requestObject = (JSONObject) parser.parse(paramString);

         String from = (String) requestObject.get("from");
         String contractId = (String) requestObject.get("contract_id");
         String contractVersion = (String) requestObject.get("version");
         String solidityCode = (String) requestObject.get("sourcecode");
         String selectedContract = (String) requestObject.get("contractName");
         String constructorParams = (String) requestObject.get("constructorParams");

         Compiler.Result result = Compiler.compile(solidityCode);
         if (result.isSuccess()) {
            String byteCode = (String) result.getByteCodeMap().get(selectedContract) + constructorParams;


            boolean success = registryManager.updateExistingContractVersion(
                    existingContractId,
                    existingVersionName,
                    contractId,
                    from,
                    contractVersion,
                    result.getMetadataMap().get(selectedContract),
                    byteCode,
                    solidityCode
            );

            if (success) {
               FullVersionInfo fvInfo
                       = registryManager.getContractVersion(contractId, contractVersion);
               return new ResponseEntity<>(buildVersionJSON(fvInfo),
                       standardHeaders,
                       HttpStatus.OK);
            } else {
               responseEntity
                       = new ResponseEntity<>(errorJSON("unable to update contract."),
                       standardHeaders,
                       HttpStatus.INTERNAL_SERVER_ERROR);
            }
         } else {
            responseEntity
                    = new ResponseEntity<>(errorJSON("Compilation failure:\n"
                    + result.getStderr()),
                    standardHeaders,
                    HttpStatus.BAD_REQUEST);
         }

         logger.error("IN UPDATE");
         logger.error(requestObject);
         logger.error("IN UPDATE");


      } catch (ParseException pe) {
         logger.warn("Exception while parsing request JSON", pe);
         responseEntity
                 = new ResponseEntity<>(errorJSON("unable to parse request."),
                 standardHeaders,
                 HttpStatus.BAD_REQUEST);
      } catch (ContractRetrievalException e) {
         return new ResponseEntity<>(errorJSON("No contract found with id: "
                 + existingContractId + " and version: " + existingVersionName),
                 standardHeaders,
                 HttpStatus.NOT_FOUND);
      } catch (Exception e) {
         logger.warn("Exception in request processing", e);
         responseEntity
                 = new ResponseEntity<>(errorJSON("unable to parse request."),
                 standardHeaders,
                 HttpStatus.INTERNAL_SERVER_ERROR);
      }
      return responseEntity;
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
                                     String from, Compiler.Result result,
                                     String solidityCode, String selectedContract, String constructorParams) throws Exception {
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
         if(contractName.equals(selectedContract)) {
            int requestID = random.nextInt(); // some request ID for JSON RPC
            String byteCode = (String) result.getByteCodeMap().get(contractName) + constructorParams;
            JSONObject sendTxrequest
                    = buildEthSendTxRequest(from,
                    requestID,
                    byteCode);
            logger.error("==========SEND TX REQUEST==========");
            logger.error(sendTxrequest);
            logger.error("==========SEND TX REQUEST==========");
            String responseString
                    = new EthDispatcher().dispatch(sendTxrequest).toJSONString();
            logger.trace("Dispatcher response: " + responseString);
            JSONObject ethResponse
                    = (JSONObject) new JSONParser().parse(responseString);

            // Build response object for this particular deployment
            JSONObject deploymentResult = new JSONObject();

            if (ethResponse.containsKey("result")) {
               String transactionHash = (String) ethResponse.get("result");
               // Now call eth_getTransactionReceipt API to get the address
               // of deployed contract
               JSONObject txReceiptRequest
                       = buildEthTxReceiptRequest(random.nextInt(), transactionHash);
               String txReceipt
                       = new EthDispatcher().dispatch(txReceiptRequest).toJSONString();
               logger.info("New contract deployed at: "
                       + extractContractAddress(txReceipt));
               boolean success
                       = registryManager.addNewContractVersion(contractId,
                       from,
                       contractVersion,
                       extractContractAddress(txReceipt),
                       result.getMetadataMap()
                               .get(contractName),
                       byteCode,
                       solidityCode);

               if (success) {
                  deploymentResult.put("contract_id", contractId);
                  deploymentResult.put("version", contractVersion);
                  deploymentResult.put("url",
                          contractEndpoint + "/"
                                  + urlEncode(contractId) + "/versions/"
                                  + urlEncode(contractVersion));
               } else {
                  deploymentResult.put("error", "deployment failed.");
               }
            } else {
               // If transactionHash == null then there was error from
               // athena, forward it to client as it is
               deploymentResult.put("error", "deployment failed.");
            }
            resultArray.add(deploymentResult);
         }
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
    * Handles the POST request for `/api/athena/contracts/compile` API.
    *
    * @param request
    *           HttpRequest object
    * @param response
    *           HttpResponse object
    *
    * @return The RESTResult object containing result of this request
    */
   @RequestMapping(path = "/api/athena/contracts/compile", method = RequestMethod.POST)
   public ResponseEntity<JSONAware>
   handlePostSource(@RequestBody String paramString) {

      // TODO: This check is fragile, find a better approach
      if (registryManager == null) {
         return new ResponseEntity<>(errorJSON("Service unavailable."),
                 standardHeaders,
                 HttpStatus.SERVICE_UNAVAILABLE);
      }

      ResponseEntity<JSONAware> responseEntity;


      try {
         JSONParser parser = new JSONParser();
         JSONObject requestObject = (JSONObject) parser.parse(paramString);

         String solidityCode = (String) requestObject.get("sourcecode");
         Compiler.Result result = Compiler.compile(solidityCode);

         if (result.isSuccess()) {
            // respond with the name of each contract and allow the user to customize the contractId that will be uploaded
            JSONArray resultArray = new JSONArray();
            for (String contractName : result.getByteCodeMap().keySet()) {
               JSONObject contractResult = new JSONObject();
               JSONObject metadata = null;
               try {
                  metadata
                          = (JSONObject) parser.parse(result.getMetadataMap().get(contractName));
               } catch (ParseException e) {
                  logger.warn("Metadata parsing failed", e);
               }

               contractResult.put("contract_name", contractName);
               contractResult.put("metadata", metadata);

               resultArray.add(contractResult);

            }
            JSONObject responseJson = new JSONObject();
            responseJson.put("data", resultArray);
            responseEntity = new ResponseEntity<>(responseJson,
                    standardHeaders,
                    HttpStatus.OK);
         } else {
            responseEntity
                    = new ResponseEntity<>(errorJSON("Compilation failure:\n"
                    + result.getStderr()),
                    standardHeaders,
                    HttpStatus.BAD_REQUEST);
         }
      } catch (ParseException pe) {
         logger.warn("Exception while parsing request JSON", pe);
         responseEntity
                 = new ResponseEntity<>(errorJSON("unable to parse request."),
                 standardHeaders,
                 HttpStatus.BAD_REQUEST);
      } catch (Exception e) {
         logger.warn("Exception in request processing", e);
         responseEntity
                 = new ResponseEntity<>(errorJSON("unable to parse request."),
                 standardHeaders,
                 HttpStatus.INTERNAL_SERVER_ERROR);
      }
      return responseEntity;
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
    *
    * @return The RESTResult object containing result of this request
    */
   @RequestMapping(path = "/api/athena/contracts", method = RequestMethod.POST)
   public ResponseEntity<JSONAware>
          handlePost(@RequestBody String paramString) {

      // TODO: This check is fragile, find a better approach
      if (registryManager == null) {
         return new ResponseEntity<>(errorJSON("Service unavailable."),
                                     standardHeaders,
                                     HttpStatus.SERVICE_UNAVAILABLE);
      }

      ResponseEntity<JSONAware> responseEntity;

      try {
         logger.debug(paramString);
         JSONParser parser = new JSONParser();
         JSONObject requestObject = (JSONObject) parser.parse(paramString);

         // First extract some fields from requestParams
         String from = (String) requestObject.get("from");
         String contractId = (String) requestObject.get("contract_id");
         String contractVersion = (String) requestObject.get("version");
         String solidityCode = (String) requestObject.get("sourcecode");
         String selectedContract = (String) requestObject.get("contractName");
         String constructorParams = (String) requestObject.get("constructorParams");

         // Check if contract with same id already exists, if yes
         // then version number must be different
         if (registryManager.hasContractVersion(contractId, contractVersion)) {
            responseEntity
               = new ResponseEntity<>(errorJSON("contract with same name and version "
                  + "already exists"), standardHeaders, HttpStatus.CONFLICT);
         } else if (registryManager.hasContract(contractId)
            && !isSameAddress(registryManager.getBriefContractInfo(contractId)
                                             .getOwnerAddress(),
                              (from))) {
            // It is a new version of
            // existing contract but from address doesn't match
            responseEntity
               = new ResponseEntity<>(errorJSON("Only original owner can deploy the"
                  + " new version of a contract"),
                                      standardHeaders,
                                      HttpStatus.FORBIDDEN);
         } else {
            // Compile the given solidity code
            Compiler.Result result = Compiler.compile(solidityCode);
            if (result.isSuccess()) {
               JSONArray resultArray = deployContracts(contractId,
                                                       contractVersion,
                                                       from,
                                                       result,
                                                       solidityCode,
                                                       selectedContract,
                                                       constructorParams);
               responseEntity = new ResponseEntity<>(resultArray,
                                                     standardHeaders,
                                                     HttpStatus.OK);
            } else {
               responseEntity
                  = new ResponseEntity<>(errorJSON("Compilation failure:\n"
                     + result.getStderr()),
                                         standardHeaders,
                                         HttpStatus.BAD_REQUEST);
            }
         }
      } catch (ParseException pe) {
         logger.warn("Exception while parsing request JSON", pe);
         responseEntity
            = new ResponseEntity<>(errorJSON("unable to parse request."),
                                   standardHeaders,
                                   HttpStatus.BAD_REQUEST);
      } catch (Exception e) {
         logger.warn("Exception in request processing", e);
         responseEntity
            = new ResponseEntity<>(errorJSON("unable to parse request."),
                                   standardHeaders,
                                   HttpStatus.INTERNAL_SERVER_ERROR);
      }
      return responseEntity;
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

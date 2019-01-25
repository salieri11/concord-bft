/*
 * Copyright (c) 2018-2019 VMware, Inc. All rights reserved. VMware Confidential
 */

package com.vmware.blockchain.services.concord;

import static com.vmware.blockchain.services.ethereum.ApiHelper.errorJson;

import java.io.UnsupportedEncodingException;
import java.net.URLEncoder;
import java.nio.charset.StandardCharsets;
import java.util.List;
import java.util.Optional;
import java.util.Random;
import java.util.UUID;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.json.simple.JSONArray;
import org.json.simple.JSONAware;
import org.json.simple.JSONObject;
import org.json.simple.parser.JSONParser;
import org.json.simple.parser.ParseException;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.stereotype.Controller;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestMethod;

import com.vmware.blockchain.common.Constants;
import com.vmware.blockchain.connections.ConnectionPoolManager;
import com.vmware.blockchain.services.ConcordControllerHelper;
import com.vmware.blockchain.services.ConcordServlet;
import com.vmware.blockchain.services.contracts.BriefContractInfo;
import com.vmware.blockchain.services.contracts.BriefVersionInfo;
import com.vmware.blockchain.services.contracts.Compiler;
import com.vmware.blockchain.services.contracts.ContractRegistryManager;
import com.vmware.blockchain.services.contracts.ContractRetrievalException;
import com.vmware.blockchain.services.contracts.FullVersionInfo;
import com.vmware.blockchain.services.ethereum.EthDispatcher;
import com.vmware.blockchain.services.profiles.DefaultProfiles;
import com.vmware.concord.Concord.ConcordResponse;

/**
 * A servlet which handles all contract management queries sent to `api/concord/contracts/*` URI.
 */
@Controller
public class ContractsServlet extends ConcordServlet {

    private static final long serialVersionUID = 1L;
    private static final Logger logger = LogManager.getLogger(ContractsServlet.class);
    private final String jsonRpc;
    private final String contractEndpoint;
    private ContractRegistryManager registryManager;
    private Random random = new Random();
    private EthDispatcher ethDispatcher;

    @Autowired
    public ContractsServlet(ContractRegistryManager registryManger, EthDispatcher ethDispatcher,
            ConnectionPoolManager connectionPoolManager, DefaultProfiles defaultProfiles) {
        super(connectionPoolManager, defaultProfiles);
        this.registryManager = registryManger;
        this.jsonRpc = Constants.JSONRPC;
        this.contractEndpoint = Constants.CONTRACTS_ENDPOINT;
        this.ethDispatcher = ethDispatcher;
    }


    /**
     * Encodes the given string as per URL encoding rules.
     *
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
     * Builds a JSON which is sent as a response object for `GET api/concord/contracts` queries.
     *
     * @return JSONObject which should be returned to client
     */
    private JSONArray buildContractsJson(UUID blockchain) {
        JSONArray cArray = new JSONArray();
        List<BriefContractInfo> cInfoList = registryManager.getAllBriefContractInfo(blockchain);

        for (BriefContractInfo cinfo : cInfoList) {
            JSONObject contract = new JSONObject();
            contract.put("contract_id", cinfo.getContractId());
            contract.put("owner", cinfo.getOwnerAddress());
            contract.put("url", contractEndpoint + "/" + urlEncode(cinfo.getContractId()));
            cArray.add(contract);
        }
        return cArray;
    }

    /**
     * Processes the `GET api/concord/contracts` request and puts the generated result inside @responseObject.
     *
     * @return the RESTResult object which contains response of this request.
     */
    @RequestMapping(method = RequestMethod.GET,
            path = {"/api/concord/contracts", "/api/blockchains/{id}/concord/contracts"})
    public ResponseEntity<JSONAware> handleGetContracts(
            @PathVariable(name = "id", required = false) Optional<UUID> id) {

        // TODO: This check is not a proper way, find a better approach
        if (registryManager == null) {
            return new ResponseEntity<>(errorJson("Service unavailable."),
                    HttpStatus.SERVICE_UNAVAILABLE);
        }

        return new ResponseEntity<>(buildContractsJson(getBlockchainId(id)), HttpStatus.OK);
    }

    /**
     * Builds a JSONObject from the given List of BriefVersionInfo objects. This JSONObject is generated as per the
     * response format of `GET api/concord/contracts/{contract_id}` request type.
     *
     * @return Returns the json object containing information about all versions present in the list.
     */
    private JSONObject buildContractJson(List<BriefVersionInfo> briefVersionInfoList) {
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
                metadata = (JSONObject) parser.parse(briefVersionInfo.getMetaData());
            } catch (ParseException e) {
                logger.warn("Metadata parsing failed", e);
            }
            version.put("version", briefVersionInfo.getVersionName());
            version.put("address", briefVersionInfo.getAddress());
            version.put("metadata", metadata);
            // TODO : is there a better way to do it?
            version.put("url", contractEndpoint + "/" + urlEncode(briefVersionInfo.getContractId()) + "/versions/"
                    + urlEncode(briefVersionInfo.getVersionName()));
            versions.add(version);
        }
        contract.put("versions", versions);
        return contract;
    }

    /**
     * Handles the `GET api/concord/contracts/{contract_id}` request.
     *
     * @param contractId The `contract_id` variable present in the request URI
     *
     * @return The RESTResult object containing result of this request
     */
    @RequestMapping(method = RequestMethod.GET,
            path = {"/api/concord/contracts/{contract_id}", "/api/blockchains/{id}/concord/contracts/{contract_id}"})
    public ResponseEntity<JSONAware> handleGetContract(@PathVariable(name = "id", required = false) Optional<UUID> id,
            @PathVariable("contract_id") String contractId) {

        // TODO: This check is not a proper way, find a better approach
        if (registryManager == null) {
            return new ResponseEntity<>(errorJson("Service unavailable."),
                    HttpStatus.SERVICE_UNAVAILABLE);
        }

        if (registryManager.hasContract(contractId, getBlockchainId(id))) {
            return new ResponseEntity<>(
                    buildContractJson(registryManager.getAllBriefVersionInfo(contractId, getBlockchainId(id))),
                    HttpStatus.OK);
        } else {
            return new ResponseEntity<>(errorJson("No contract found with id: " + contractId),
                    HttpStatus.NOT_FOUND);
        }
    }

    /**
     * Builds a JSONObject representing the given version of the given contract object. The JSONObject is generated to
     * follow the response format of `GET api/concord/contracts/{contract_id}/versions/{version}` request.
     *
     * @return The generated JSON with all details of the specific version
     */
    private JSONObject buildVersionJson(FullVersionInfo versionInfo) {
        JSONObject versionJson = new JSONObject();
        JSONParser parser = new JSONParser();
        JSONObject metadata = null;
        try {
            metadata = (JSONObject) parser.parse(versionInfo.getMetaData());
        } catch (ParseException e) {
            logger.warn("Metadata parsing failure: ", e);
            metadata = new JSONObject();
        }
        versionJson.put("contract_id", versionInfo.getContractId());
        versionJson.put("owner", versionInfo.getOwnerAddress());
        versionJson.put("version", versionInfo.getVersionName());
        versionJson.put("metadata", metadata);
        versionJson.put("address", versionInfo.getAddress());
        versionJson.put("bytecode", versionInfo.getByteCode());
        versionJson.put("sourcecode", versionInfo.getSourceCode());
        return versionJson;
    }

    /**
     * Handles the `GET api/concord/contracts/{contract_id}/versions/{version }` request.
     *
     * @param contractId The value of `{contract_id}` from URI
     * @param contractVersion The value of `{version}` from URI
     * @return The RESTResult object containing result of this request
     */
    @RequestMapping(method = RequestMethod.GET, path = {"/api/concord/contracts/{contract_id}/versions/{version_id}",
            "/api/blockchains/{id}/concord/contracts/{contract_id}/versions/{version_id}"})
    public ResponseEntity<JSONAware> handleGetVersion(@PathVariable(name = "id", required = false) Optional<UUID> id,
            @PathVariable("contract_id") String contractId,
            @PathVariable("version_id") String contractVersion) {

        // TODO: This check is not a proper way, find a better approach
        if (registryManager == null) {
            return new ResponseEntity<>(errorJson("Service unavailable."),
                    HttpStatus.SERVICE_UNAVAILABLE);
        }

        try {
            FullVersionInfo fvInfo =
                    registryManager.getContractVersion(contractId, contractVersion, getBlockchainId(id));
            return new ResponseEntity<>(buildVersionJson(fvInfo), HttpStatus.OK);
        } catch (ContractRetrievalException e) {
            return new ResponseEntity<>(
                    errorJson("No contract found with id: " + contractId + " and version: " + contractVersion),
                    HttpStatus.NOT_FOUND);
        }
    }

    /**
     * Builds a JSONObject which represents a request of type `eth_sendTransaction`. (Ethereum send transaction).
     *
     * @param from Address from which new contract creation request was sent
     * @param id Request id
     * @param byteCode bytecode of compiled contract
     * @return JSON object of ethereum request
     */
    private JSONObject buildEthSendTxRequest(String from, long id, String byteCode) {
        // Now make a eth_sendTransaction request json object
        final JSONObject ethRequest = new JSONObject();
        final JSONArray paramsArray = new JSONArray();
        final JSONObject transaction = new JSONObject();
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
     * Builds an Ethereum Get Transaction Receipt (eth_getTransactionReceipt) request JSON object.
     *
     * @param id The request id
     * @param transactionHash The hash of transction whose receipt is needed
     * @return the JSON request object
     */
    private JSONObject buildEthTxReceiptRequest(long id, String transactionHash) {
        // Now make a eth_sendTransaction request json object
        final JSONObject ethRequest = new JSONObject();
        final JSONArray paramsArray = new JSONArray();
        ethRequest.put("id", id);
        ethRequest.put("jsonrpc", jsonRpc);
        ethRequest.put("method", "eth_getTransactionReceipt");
        paramsArray.add(transactionHash);
        ethRequest.put("params", paramsArray);
        return ethRequest;
    }

    /**
     * Extracts the contracts address from the response of the `eth_getTransactionReceipt` request.
     *
     * @param jsonStr The string representing JSONObject of the response of `eth_getTransactionReceipt` request.
     * @return The value of key "result" from given JSON string which ideally represents the address at which contract
     *         is deployed.
     */
    private String extractContractAddress(String jsonStr) throws ParseException {
        JSONParser parser = new JSONParser();
        JSONObject json = (JSONObject) parser.parse(jsonStr);
        JSONObject result = (JSONObject) json.get("result");
        String address = (String) result.get("contractAddress");
        return address;
    }

    /**
     * Handles the request for `PUT api/concord/contracts/{contract_id}/versions/{version }` request.
     *
     * @param existingContractId The value of `{contract_id}` from URI
     * @param existingVersionName The value of `{version}` from URI
     * @param paramString HttpRequest object
     * @return The RESTResult object containing result of this request
     */
    @RequestMapping(method = RequestMethod.PUT, path = {"/api/concord/contracts/{contract_id}/versions/{version_id}",
            "/api/blockchains/{id}/concord/contracts/{contract_id}/versions/{version_id}"})
    public ResponseEntity<JSONAware> handleUpdateVersion(@RequestBody String paramString,
            @PathVariable(name = "id", required = false) Optional<UUID> id,
            @PathVariable("contract_id") String existingContractId,
            @PathVariable("version_id") String existingVersionName) {

        // TODO: This check is not a proper way, find a better approach
        if (registryManager == null) {
            return new ResponseEntity<>(errorJson("Service unavailable."),
                    HttpStatus.SERVICE_UNAVAILABLE);
        }

        ResponseEntity<JSONAware> responseEntity;

        try {
            JSONParser parser = new JSONParser();
            JSONObject requestObject = (JSONObject) parser.parse(paramString);

            String from = (String) requestObject.get("from");
            String contractId = (String) requestObject.get("contract_id");
            String contractVersion = (String) requestObject.get("version");
            String solidityCode = (String) requestObject.get("sourcecode");
            String selectedContract = (String) requestObject.get("contractName");
            String constructorParams = (String) requestObject.get("constructorParams");
            if (registryManager.hasContractVersion(contractId, contractVersion, getBlockchainId(id))) {
                responseEntity =
                        new ResponseEntity<>(errorJson("contract with same name and version " + "already exists"),
                                HttpStatus.CONFLICT);
            } else {
                Compiler.Result result = Compiler.compile(solidityCode);
                if (result.isSuccess()) {
                    String byteCode = result.getByteCodeMap().get(selectedContract) + constructorParams;

                    boolean success = registryManager.updateExistingContractVersion(existingContractId,
                            existingVersionName, contractId, from, contractVersion,
                            result.getMetadataMap().get(selectedContract), solidityCode, getBlockchainId(id));

                    if (success) {
                        FullVersionInfo fvInfo =
                                registryManager.getContractVersion(contractId, contractVersion, getBlockchainId(id));
                        return new ResponseEntity<>(buildVersionJson(fvInfo), HttpStatus.OK);
                    } else {
                        responseEntity = new ResponseEntity<>(errorJson("unable to update contract."),
                                HttpStatus.INTERNAL_SERVER_ERROR);
                    }
                } else {
                    responseEntity = new ResponseEntity<>(errorJson("Compilation failure:\n" + result.getStderr()),
                            HttpStatus.BAD_REQUEST);
                }
            }
        } catch (ParseException pe) {
            logger.warn("Exception while parsing request JSON", pe);
            responseEntity = new ResponseEntity<>(errorJson("unable to parse request."),
                    HttpStatus.BAD_REQUEST);
        } catch (ContractRetrievalException e) {
            return new ResponseEntity<>(errorJson(
                    "No contract found with id: " + existingContractId + " and version: " + existingVersionName),
                    HttpStatus.NOT_FOUND);
        } catch (Exception e) {
            logger.warn("Exception in request processing", e);
            responseEntity = new ResponseEntity<>(errorJson("unable to parse request."),
                    HttpStatus.INTERNAL_SERVER_ERROR);
        }
        return responseEntity;
    }

    /**
     * Deploys the given set of compiled contracts to concord. A common way of deploying a new contract to concord is by
     * sending a `eth_sendTransaction` request to Concord with contract code in that request. Here, we utilize the same
     * functionality (since it is already implemented in helen). We make a `eth_sendTransaction` request for every
     * contract and send them to concord in a loop. After every request we call `eth_getTransactionReceipt` to either
     * get the address at which contract was deployed or the error in case of failure.
     *
     * @param blockchain The UUID of the blockchain that owns this contract
     * @param contractId The unique contract_id provided by the user
     * @param contractVersion The version number for this contract provided by the user.
     * @param from Address of the account which initiated this request.
     * @param result Compiler.Result object which contains all compilation outputs generated by compiling given solidity
     *        file
     * @param solidityCode The actual solidity code which was compiled
     * @param selectedContract The name of the contract in the file which should be deployed
     * @param constructorParams The encoded constructor parameters for the contract to be deployed
     * @return The JSONArray containing results of deployment of each contract present in solidity code.
     */
    private JSONObject deployContracts(UUID blockchain, String contractId, String contractVersion, String from,
            Compiler.Result result, String solidityCode, String selectedContract, String constructorParams)
            throws Exception {
        // Since EthDispatcher already has the code of handling ethereum
        // requests we just build a JSON object representing an ethereum
        // request (as if it was received like a normal ethereum JSON RPC)
        // and forward it to EthDispatcher.
        int requestId = random.nextInt(); // some request ID for JSON RPC
        boolean hasContract = result.getByteCodeMap().containsKey(selectedContract);
        // Build response object for this particular deployment
        JSONObject deploymentResult = new JSONObject();
        if (hasContract) {
            String byteCode = result.getByteCodeMap().get(selectedContract) + constructorParams;
            JSONObject sendTxrequest = buildEthSendTxRequest(from, requestId, byteCode);
            String responseString = ethDispatcher.dispatch(Optional.of(blockchain), sendTxrequest).toJSONString();
            logger.trace("Dispatcher response: " + responseString);
            JSONObject ethResponse = (JSONObject) new JSONParser().parse(responseString);



            if (ethResponse.containsKey("result")) {
                String transactionHash = (String) ethResponse.get("result");
                // Now call eth_getTransactionReceipt API to get the address
                // of deployed contract
                JSONObject txReceiptRequest = buildEthTxReceiptRequest(random.nextInt(), transactionHash);
                String txReceipt = ethDispatcher.dispatch(Optional.of(blockchain), txReceiptRequest).toJSONString();
                logger.info("New contract deployed at: " + extractContractAddress(txReceipt));
                boolean success = registryManager.addNewContractVersion(contractId, from, contractVersion,
                        extractContractAddress(txReceipt), result.getMetadataMap().get(selectedContract), byteCode,
                        solidityCode, blockchain);

                if (success) {
                    deploymentResult.put("contract_id", contractId);
                    deploymentResult.put("version", contractVersion);
                    deploymentResult.put("url",
                            contractEndpoint + "/" + urlEncode(contractId) + "/versions/" + urlEncode(contractVersion));
                } else {
                    deploymentResult.put("error", "deployment failed.");
                }
            } else {
                // If transactionHash == null then there was error from
                // concord, forward it to client as it is
                deploymentResult.put("error", "deployment failed.");
            }
        } else {
            deploymentResult.put("error", "Selected contract not found.");
        }

        return deploymentResult;
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
     * Handles the POST request for `/api/concord/contracts/compile` API.
     *
     * @return The RESTResult object containing result of this request
     */
    @RequestMapping(path = "/api/concord/contracts/compile", method = RequestMethod.POST)
    public ResponseEntity<JSONAware> handlePostSource(@RequestBody String paramString) {

        // TODO: This check is fragile, find a better approach
        if (registryManager == null) {
            return new ResponseEntity<>(errorJson("Service unavailable."),
                    HttpStatus.SERVICE_UNAVAILABLE);
        }

        ResponseEntity<JSONAware> responseEntity;


        try {
            JSONParser parser = new JSONParser();
            JSONObject requestObject = (JSONObject) parser.parse(paramString);

            String solidityCode = (String) requestObject.get("sourcecode");
            Compiler.Result result = Compiler.compile(solidityCode);

            if (result.isSuccess()) {
                // respond with the name of each contract and allow the user to customize the contractId that will be
                // uploaded
                JSONArray resultArray = new JSONArray();
                for (String contractName : result.getByteCodeMap().keySet()) {
                    JSONObject contractResult = new JSONObject();
                    JSONObject metadata = null;
                    try {
                        metadata = (JSONObject) parser.parse(result.getMetadataMap().get(contractName));
                    } catch (ParseException e) {
                        logger.warn("Metadata parsing failed", e);
                    }

                    contractResult.put("contract_name", contractName);
                    contractResult.put("metadata", metadata);

                    resultArray.add(contractResult);

                }
                JSONObject responseJson = new JSONObject();
                responseJson.put("data", resultArray);
                responseEntity = new ResponseEntity<>(responseJson, HttpStatus.OK);
            } else {
                responseEntity = new ResponseEntity<>(errorJson("Compilation failure:\n" + result.getStderr()),
                        HttpStatus.BAD_REQUEST);
            }
        } catch (ParseException pe) {
            logger.warn("Exception while parsing request JSON", pe);
            responseEntity = new ResponseEntity<>(errorJson("unable to parse request."),
                    HttpStatus.BAD_REQUEST);
        } catch (Exception e) {
            logger.warn("Exception in request processing", e);
            responseEntity = new ResponseEntity<>(errorJson("unable to parse request."),
                    HttpStatus.INTERNAL_SERVER_ERROR);
        }
        return responseEntity;
    }

    /**
     * Handles the POST request for `/api/concord/contracts` API. A POST request expects a solidity code as a part of
     * the POST body. The POST body must be a JSON having a key "params" and value being a strings of solidity contract
     * code.
     *
     * @return The RESTResult object containing result of this request
     */
    @RequestMapping(path = {"/api/concord/contracts", "/api/blockchains/{id}/concord/contracts"},
            method = RequestMethod.POST)
    public ResponseEntity<JSONAware> handlePost(@PathVariable(name = "id", required = false) Optional<UUID> id,
            @RequestBody String paramString) {

        // TODO: This check is fragile, find a better approach
        if (registryManager == null) {
            return new ResponseEntity<>(errorJson("Service unavailable."),
                    HttpStatus.SERVICE_UNAVAILABLE);
        }

        final ConcordControllerHelper helper = getHelper(id);

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
            if (registryManager.hasContractVersion(contractId, contractVersion, getBlockchainId(id))) {
                responseEntity = new ResponseEntity<>(
                        errorJson("contract with same name and version " + "already exists"), HttpStatus.CONFLICT);
            } else if (registryManager.hasContract(contractId, getBlockchainId(id)) && !isSameAddress(
                    registryManager.getBriefContractInfo(contractId, getBlockchainId(id)).getOwnerAddress(), (from))) {
                // It is a new version of
                // existing contract but from address doesn't match
                responseEntity = new ResponseEntity<>(
                        errorJson("Only original owner can deploy the" + " new version of a contract"),
                        HttpStatus.FORBIDDEN);
            } else {
                // Compile the given solidity code
                Compiler.Result result = Compiler.compile(solidityCode);
                if (result.isSuccess()) {
                    JSONObject resultObject = deployContracts(helper.getBlockchain(), contractId, contractVersion, from,
                            result, solidityCode, selectedContract, constructorParams);
                    responseEntity = new ResponseEntity<>(resultObject, HttpStatus.OK);
                } else {
                    responseEntity = new ResponseEntity<>(errorJson("Compilation failure:\n" + result.getStderr()),
                            HttpStatus.BAD_REQUEST);
                }
            }
        } catch (ParseException pe) {
            logger.warn("Exception while parsing request JSON", pe);
            responseEntity = new ResponseEntity<>(errorJson("unable to parse request."),
                    HttpStatus.BAD_REQUEST);
        } catch (Exception e) {
            logger.warn("Exception in request processing", e);
            responseEntity = new ResponseEntity<>(errorJson("unable to parse request."),
                    HttpStatus.INTERNAL_SERVER_ERROR);
        }
        return responseEntity;
    }


    @Override
    public JSONAware parseToJson(ConcordResponse concordResponse) {
        throw new UnsupportedOperationException("parseToJSON method is not supported in ContractServlet class");
    }

}

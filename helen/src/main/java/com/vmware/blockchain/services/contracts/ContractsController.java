/*
 * Copyright (c) 2018-2019 VMware, Inc. All rights reserved. VMware Confidential
 */

package com.vmware.blockchain.services.contracts;

import static com.vmware.blockchain.services.ethereum.ApiHelper.errorJson;

import java.util.List;
import java.util.Optional;
import java.util.Random;
import java.util.UUID;
import java.util.stream.Collectors;
import java.util.stream.Stream;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.json.simple.JSONArray;
import org.json.simple.JSONAware;
import org.json.simple.JSONObject;
import org.json.simple.parser.JSONParser;
import org.json.simple.parser.ParseException;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.security.access.prepost.PreAuthorize;
import org.springframework.stereotype.Controller;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestMethod;
import org.springframework.web.util.UriComponents;
import org.springframework.web.util.UriComponentsBuilder;

import com.fasterxml.jackson.annotation.JsonInclude;
import com.fasterxml.jackson.annotation.JsonInclude.Include;
import com.vmware.blockchain.auth.AuthHelper;
import com.vmware.blockchain.common.BadRequestException;
import com.vmware.blockchain.common.ConflictException;
import com.vmware.blockchain.common.Constants;
import com.vmware.blockchain.common.ErrorCode;
import com.vmware.blockchain.common.ForbiddenException;
import com.vmware.blockchain.common.InternalFailureException;
import com.vmware.blockchain.common.NotFoundException;
import com.vmware.blockchain.connections.ConnectionPoolManager;
import com.vmware.blockchain.services.ConcordControllerHelper;
import com.vmware.blockchain.services.ConcordServlet;
import com.vmware.blockchain.services.ethereum.EthDispatcher;
import com.vmware.blockchain.services.profiles.DefaultProfiles;
import com.vmware.concord.Concord.ConcordResponse;

import lombok.Data;

/**
 * A servlet which handles all contract management queries sent to `api/concord/contracts/*` URI.
 */
@Controller
public class ContractsController extends ConcordServlet {

    private static final long serialVersionUID = 1L;
    private static final Logger logger = LogManager.getLogger(ContractsController.class);
    private static final int DEFAULT_RUNS = 200;
    private final String jsonRpc;
    private ContractService contractService;
    private Random random = new Random();
    private EthDispatcher ethDispatcher;
    private AuthHelper authHelper;
    @Value("${compilerService.url}")
    private String compilerServiceUrl;

    @Autowired
    public ContractsController(ContractService registryManger, EthDispatcher ethDispatcher,
            ConnectionPoolManager connectionPoolManager, DefaultProfiles defaultProfiles,
            AuthHelper authHelper, @Value("${compilerService.url}") String compilerServiceUrl) {
        super(connectionPoolManager, defaultProfiles);
        this.contractService = registryManger;
        this.jsonRpc = Constants.JSONRPC;
        this.ethDispatcher = ethDispatcher;
        this.authHelper = authHelper;
        this.compilerServiceUrl = compilerServiceUrl;
    }


    // method used in contract get mapper
    private BriefContractInfo setUrl(BriefContractInfo bc, UUID id, String contractName) {
        String url = toUrlString(id, contractName);
        bc.setUrl(url);
        return bc;
    }


    // Methods to encode a contract, and a contract with version to a url
    private String toUrlString(UUID id, String contractName) {
        // Need to url encode the contract name, since it may have spaces
        UriComponents contractSeg = UriComponentsBuilder.newInstance().pathSegment(contractName).build().encode();
        return UriComponentsBuilder.newInstance().path("/api/blockchains/{bid}/concord/contracts")
                .uriComponents(contractSeg).buildAndExpand(id).toString();
    }

    private String toUrlString(UUID id, String contractName, String contractVersion) {
        UriComponents versionSeg = UriComponentsBuilder.newInstance().pathSegment(contractVersion).build().encode();

        return UriComponentsBuilder.newInstance().path(toUrlString(id, contractName))
                .pathSegment("versions").uriComponents(versionSeg)
                .build().toString();
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

    private int getRuns(boolean isOptimize, String runParam) {
        int runs = DEFAULT_RUNS;
        if (isOptimize) {
            try {
                runs = Integer.parseInt(runParam);
            } catch (NumberFormatException e) {
                throw new BadRequestException(ErrorCode.BAD_NUMBER_FORMAT, runParam);
            }
        }
        return runs;
    }

    /**
     * Processes the `GET api/concord/contracts` request and puts the generated result inside @responseObject.
     *
     * @return the RESTResult object which contains response of this request.
     */
    @RequestMapping(method = RequestMethod.GET,
            path = {"/api/concord/contracts", "/api/blockchains/{id}/concord/contracts"})
    @PreAuthorize("@authHelper.canAccessChain(#id)")
    public ResponseEntity<List<BriefContractInfo>> handleGetContracts(
            @PathVariable(name = "id", required = false) Optional<UUID> id) {

        return new ResponseEntity<>(
                contractService.list(getBlockchainId(id)).stream()
                    .map(BriefContractInfo::new)
                    .map(bc -> setUrl(bc, getBlockchainId(id), bc.getContractId()))
                    .collect(Collectors.toList()),
                HttpStatus.OK);
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
    @PreAuthorize("@authHelper.canAccessChain(#id)")
    public ResponseEntity<BriefContractInfo> handleGetContract(
            @PathVariable(name = "id", required = false) Optional<UUID> id,
            @PathVariable("contract_id") String contractId) {

        List<Contract> contracts = contractService.listByName(contractId, getBlockchainId(id));
        if (contracts.isEmpty()) {
            throw new NotFoundException(ErrorCode.CONTRACT_NOT_FOUND, contractId);
        }
        // get the version list
        List<BriefVersionInfo> versions = contracts.stream().map(BriefVersionInfo::new).collect(Collectors.toList());

        // create the new response, and set versions
        BriefContractInfo response = new BriefContractInfo(contracts.get(0));
        response.setVersions(versions);
        return new ResponseEntity<>(response, HttpStatus.OK);
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
    @PreAuthorize("@authHelper.canAccessChain(#id)")
    public ResponseEntity<FullVersionInfo> handleGetVersion(
            @PathVariable(name = "id", required = false) Optional<UUID> id,
            @PathVariable("contract_id") String contractId, @PathVariable("version_id") String contractVersion) {

        List<Contract> contracts = contractService.getContractVersion(contractId, contractVersion, getBlockchainId(id));

        if (contracts.isEmpty()) {
            throw new NotFoundException(ErrorCode.CONTRACT_VERSION_NOT_FOUND, contractId, contractVersion);
        }

        return new ResponseEntity<>(new FullVersionInfo(contracts.get(0)), HttpStatus.OK);
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

    @Data
    @JsonInclude(Include.NON_NULL)
    private static class PutRequestBody {
        String contractId;
        String compilerVersion;
        String sourcecode;
        String contractName;
        Boolean isOptimize;
        String runs;
    }

    /**
     * Handles the request for `PUT api/concord/contracts/{contract_id}/versions/{version}` request.
     *
     * @param existingContractId The value of `{contract_id}` from URI
     * @param body HttpRequest object
     * @return The RESTResult object containing result of this request
     */
    @RequestMapping(method = RequestMethod.PUT, path = {"/api/concord/contracts/{contract_address}",
            "/api/blockchains/{id}/concord/contracts/{contract_address}"})
    @PreAuthorize("@authHelper.canAccessChain(#id) "
                  + "&& @authHelper.isDeveloper()")
    public ResponseEntity<FullVersionInfo> handleUpdateVersion(@RequestBody PutRequestBody body,
            @PathVariable(name = "id", required = false) Optional<UUID> id,
            @PathVariable("contract_address") String existingContractId) {

        String contractId = body.getContractId();
        String solidityCode = body.getSourcecode();
        String selectedContract = body.getContractName();
        String compilerVersion = body.getCompilerVersion();
        String existingVersionName = "1";
        final boolean isOptimize = body.getIsOptimize() == null ? false : body.getIsOptimize();
        int runs = getRuns(isOptimize, body.getRuns());

        // verify parameters that need to be non-null
        if (Stream.of(contractId, solidityCode, selectedContract, compilerVersion)
                .anyMatch(s -> s == null)) {
            throw new BadRequestException(ErrorCode.BAD_REQUEST);
        }


        List<Contract> contracts = contractService.getContractVersion(
                existingContractId,
                existingVersionName, getBlockchainId(id));
        if (contracts.isEmpty()) {
            throw new NotFoundException(ErrorCode.CONTRACT_VERSION_NOT_FOUND,
                    existingContractId, existingVersionName);
        }
        String existingBytecode = contracts.get(0).getBytecode();
        Compiler.Result result =
                Compiler.compile(solidityCode, compilerVersion, compilerServiceUrl, isOptimize, runs);
        boolean verified = Compiler.verify(solidityCode, compilerVersion, compilerServiceUrl, existingBytecode,
                selectedContract, isOptimize, runs);
        if (!verified) {
            throw new BadRequestException(
                    "Verification failure: The uploaded contract does not match this contract.");
        }
        List<Contract> verifiedcontracts = contractService.getContractVersion(
                contractId,
                existingVersionName, getBlockchainId(id));
        if (!verifiedcontracts.isEmpty() && !contractId.equals(existingContractId)) {
            throw new BadRequestException(
                    "Verification failure: The contract name already exists.");
        }
        Contract c = contractService.updateExistingContractVersion(existingContractId, existingVersionName,
                contractId, result.getMetadataMap().get(selectedContract), solidityCode, getBlockchainId(id));
        return new ResponseEntity<>(new FullVersionInfo(c), HttpStatus.OK);

    }

    @Data
    private static class PostRequestBody {
        String contractId;
        String version;
        String from;
        String sourcecode;
        String contractName;
        String constructorParams;
        String compilerVersion;
        Boolean isOptimize;
        String runs;
    }

    @Data
    @JsonInclude(Include.NON_EMPTY)
    private static class PostResult {
        String contractId;
        String version;
        String url;
        String error;

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
    private PostResult deployContracts(UUID blockchain, String contractId, String contractVersion, String from,
            Compiler.Result result, String solidityCode, String selectedContract, String constructorParams) {
        // Since EthDispatcher already has the code of handling ethereum
        // requests we just build a JSON object representing an ethereum
        // request (as if it was received like a normal ethereum JSON RPC)
        // and forward it to EthDispatcher.
        int requestId = random.nextInt(); // some request ID for JSON RPC
        boolean hasContract = result.getByteCodeMap().containsKey(selectedContract);
        // Build response object for this particular deployment
        PostResult deploymentResult = new PostResult();
        try {
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
                    contractService.addNewContractVersion(contractId, from, contractVersion,
                            extractContractAddress(txReceipt), result.getMetadataMap().get(selectedContract), byteCode,
                            solidityCode, blockchain);

                    deploymentResult.setContractId(contractId);
                    deploymentResult.setVersion(contractVersion);
                    deploymentResult.setUrl(toUrlString(blockchain, contractId, contractVersion));
                    deploymentResult.setError(null);
                } else {
                    // If transactionHash == null then there was error from
                    // concord, forward it to client as it is
                    deploymentResult.setError("deployment failed.");
                }
            } else {
                deploymentResult.setError("Selected contract not found.");
            }
        } catch (Exception e) {
            throw new InternalFailureException(e, ErrorCode.INTERNAL_ERROR);
        }

        return deploymentResult;
    }

    /**
     * Handles the POST request for `/api/concord/contracts/compile` API.
     * TODO: This is not documented in Swagger.  I'd like to get rid of all JsonObject stuff
     * as well.
     *
     * @return The RESTResult object containing result of this request
     */
    @RequestMapping(path = "/api/concord/contracts/compile", method = RequestMethod.POST)
    @PreAuthorize("@authHelper.isDeveloper()")
    public ResponseEntity<JSONAware> handlePostSource(@RequestBody String paramString) {

        // TODO: See if we can convert this into the new style
        ResponseEntity<JSONAware> responseEntity;


        try {
            JSONParser parser = new JSONParser();
            JSONObject requestObject = (JSONObject) parser.parse(paramString);

            String solidityCode = (String) requestObject.get("sourcecode");
            String compilerVersion = (String) requestObject.get("compiler_version");
            Object o = requestObject.get("is_optimize");
            boolean isOptimize = o == null ? false : (boolean) o;
            int runs = getRuns(isOptimize, (String) requestObject.get("runs"));
            Compiler.Result result = Compiler.compile(solidityCode, compilerVersion, compilerServiceUrl,
                    isOptimize, runs);

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
    @PreAuthorize("@authHelper.canAccessChain(#id) "
                  + "&& @authHelper.isDeveloper()")
    public ResponseEntity<PostResult> handlePost(@PathVariable(name = "id", required = false) Optional<UUID> id,
                @RequestBody PostRequestBody body) {

        final ConcordControllerHelper helper = getHelper(id);

        // First extract some fields from requestParams
        final String from = body.getFrom();
        final String contractId = body.getContractId();
        final String contractVersion = body.getVersion();
        final String solidityCode = body.getSourcecode();
        final String selectedContract = body.getContractName();
        final String constructorParams = body.getConstructorParams() == null ? "" : body.getConstructorParams();
        final String compilerVersion = body.getCompilerVersion();
        final boolean isOptimize = body.getIsOptimize() == null ? false : body.getIsOptimize();
        final int runs = getRuns(isOptimize, body.getRuns());

        // verify parameters that need to be non-null
        if (Stream.of(from, contractId, contractVersion, solidityCode, selectedContract, compilerVersion)
                .anyMatch(s -> s == null)) {
            throw new BadRequestException(ErrorCode.BAD_REQUEST);
        }

        // Get all contracts with this name
        List<Contract> contracts = contractService.listByName(contractId, getBlockchainId(id));
        // If there are any, the version name must be different
        if (contracts.stream().anyMatch(c -> c.getVersionName().equals(contractVersion))) {
            throw new ConflictException(ErrorCode.DUPLICATE_CONTRACT_ID, contractId, contractVersion);
        }
        // If there are contracts, make sure the own is the same
        if (!contracts.isEmpty() && !isSameAddress(contracts.get(0).getOwner(), from)) {
            throw new ForbiddenException(ErrorCode.CONTRACT_NOT_OWNER);
        }

        // Compile the given solidity code
        Compiler.Result result = Compiler.compile(solidityCode, compilerVersion, compilerServiceUrl,
                isOptimize, runs);

        // Bad request error if unsuccessful
        if (!result.isSuccess()) {
            throw new BadRequestException(ErrorCode.CONTRACT_COMPILE_FAILED, result.getStderr());
        }

        PostResult r = deployContracts(helper.getBlockchain(), contractId, contractVersion, from,
                result, solidityCode, selectedContract, constructorParams);
        if (r.getError() != null) {
            throw new BadRequestException(ErrorCode.CONTRACT_DEPLOY_FAILED, r.getContractId());
        }
        return new ResponseEntity<>(r, HttpStatus.OK);
    }


    @Override
    public JSONAware parseToJson(ConcordResponse concordResponse) {
        throw new BadRequestException(ErrorCode.BAD_REQUEST);
    }

    /**
     * Processes the `GET api/concord/contracts/compiler_versions` request
     * and puts the generated result inside @responseObject.
     *
     * @return the RESTResult object which contains response of this request.
     */
    @RequestMapping(method = RequestMethod.GET,
                    path = "/api/concord/contracts/compiler_versions")
    @PreAuthorize("@authHelper.isDeveloper()")
    public ResponseEntity<JSONAware> handleGetCompilerVersions() {
        return new ResponseEntity<>(Compiler.getCompilerVersions(compilerServiceUrl),
                                    HttpStatus.OK);
    }
}

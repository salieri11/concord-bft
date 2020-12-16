/*
 * Copyright (c) 2018-2019 VMware, Inc. All rights reserved. VMware Confidential
 */

package com.vmware.blockchain.services.ethereum;

import java.util.Optional;
import java.util.UUID;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.json.simple.JSONArray;
import org.json.simple.JSONObject;
import org.json.simple.parser.JSONParser;

import com.google.protobuf.ByteString;
import com.vmware.blockchain.auth.AuthHelper;
import com.vmware.blockchain.common.Constants;
import com.vmware.blockchain.common.ErrorCodeType;
import com.vmware.blockchain.connections.ConnectionPoolManager;
import com.vmware.blockchain.services.contracts.ContractService;
import com.vmware.blockchain.services.profiles.DefaultProfiles;
import com.vmware.blockchain.services.profiles.ProfilesService;
import com.vmware.concord.Concord;
import com.vmware.concord.Concord.EthRequest;
import com.vmware.concord.Concord.EthRequest.EthMethod;
import com.vmware.concord.Concord.EthResponse;

/**
 * <p>
 * This handler is used to service eth_sendTransaction and eth_call POST requests. These are bundled together here
 * because functionally, the processing for both these request types is similar.
 * </p>
 */
public class EthSendTxHandler extends AbstractEthRpcHandler {

    private static Logger logger = LogManager.getLogger(EthSendTxHandler.class);
    private boolean isInternalContract;
    private ContractService registryManager;
    private ConnectionPoolManager connectionPoolManager;
    private DefaultProfiles defaultProfiles;
    private ProfilesService profilesRegistryManager;
    private Optional<UUID> blockchain;
    private AuthHelper authHelper;

    /**
     * Send transaction constructor.
     */
    public EthSendTxHandler(ConnectionPoolManager connectionPoolManager, DefaultProfiles defaultProfiles,
            ProfilesService profilesRegistryManager,
            Optional<UUID> blockchain, ContractService registryManager, boolean isInternalContract,
            AuthHelper authHelper) {
        // If isInternalContract is true, the handler is processing a contract created from the Helen UI.
        this.isInternalContract = isInternalContract;
        this.registryManager = registryManager;
        this.connectionPoolManager = connectionPoolManager;
        this.profilesRegistryManager = profilesRegistryManager;
        this.defaultProfiles = defaultProfiles;
        this.blockchain = blockchain;
        this.authHelper = authHelper;
    }

    /**
     * Builds the Concord request builder. Extracts the method name, from, to, data and value fields from the request
     * and uses it to set up an Concord Request builder with an EthRequest.
     *
     * <p>'from' is mandatory for send tx and 'to' is mandatory for call contract.
     *
     * @param concordRequestBuilder Object in which request is built
     * @param requestJson Request parameters passed by the user
     * @return Always true - send the request.
     */
    @Override
    public boolean buildRequest(Concord.ConcordRequest.Builder concordRequestBuilder, JSONObject requestJson)
            throws EthRpcHandlerException, ApiHelper.HexParseException, RlpParser.RlpEmptyException {

        Concord.EthRequest ethRequest = null;
        EthRequest.Builder b = initializeRequestObject(requestJson);
        String method = EthDispatcher.getEthMethodName(requestJson);

        JSONArray params = extractRequestParams(requestJson);
        if (method.equals(Constants.SEND_TRANSACTION_NAME)) {
            b.setMethod(EthMethod.SEND_TX);
            buildRequestFromObject(b, (JSONObject) params.get(0), true /* isSendTx */);
        } else if (method.equals(Constants.SEND_RAWTRANSACTION_NAME)) {
            b.setMethod(EthMethod.SEND_TX);
            buildRequestFromString(b, (String) params.get(0));
        } else {
            b.setMethod(EthMethod.CALL_CONTRACT);
            buildRequestFromObject(b, (JSONObject) params.get(0), false /* isSendTx */);
            // add "block" parameter
            if (params.size() == 2) {
                long blockNumber = ApiHelper.parseBlockNumber((String) params.get(1));
                if (blockNumber != -1) {
                    b.setBlockNumber(blockNumber);
                }
            }
        }

        ethRequest = b.build();
        concordRequestBuilder.addEthRequest(ethRequest);
        return true;
    }

    private void buildRequestFromObject(EthRequest.Builder b, JSONObject obj, boolean isSendTx)
            throws EthRpcHandlerException, ApiHelper.HexParseException {

        if (obj.containsKey("from")) {
            String from = (String) obj.get("from");
            ByteString fromAddr = ApiHelper.hexStringToBinary(from);
            b.setAddrFrom(fromAddr);
        } else if (isSendTx) {
            // TODO: if we allow r/s/v signature fields, we don't have to require
            // 'from' when they're present
            logger.error("From field missing in params");
            throw new EthRpcHandlerException(ErrorCodeType.FROM_UNSPECIFIED);
        }

        if (obj.containsKey("to")) {
            String to = (String) obj.get("to");
            ByteString toAddr = ApiHelper.hexStringToBinary(to);
            b.setAddrTo(toAddr);
        } else if (!isSendTx) {
            logger.error("To field missing in params");
            throw new EthRpcHandlerException(ErrorCodeType.TO_UNSPECIFIED);
        }

        if (obj.containsKey("data")) {
            String data = (String) obj.get("data");
            if (data != null) {
                ByteString dataBytes = ApiHelper.hexStringToBinary(data);
                b.setData(dataBytes);
            }
        }

        if (obj.containsKey("value")) {
            String value = (String) obj.get("value");
            if (value != null) {
                ByteString valueBytes = ApiHelper.hexStringToBinary(ApiHelper.padZeroes(value));
                b.setValue(valueBytes);
            }
        }

        if (obj.containsKey("gas")) {
            String gas = (String) obj.get("gas");
            if (gas != null) {
                if (gas.startsWith("0x")) {
                    b.setGas(Long.valueOf(gas.substring(2), 16));
                } else {
                    b.setGas(Long.valueOf(gas));
                }
            }
        }

        // TODO: add gasPrice, nonce, r, s, v
        // (no, rsv are not specified in the doc, but why not?)
    }

    private void buildRequestFromString(EthRequest.Builder b, String rlp)
            throws EthRpcHandlerException, ApiHelper.HexParseException, RlpParser.RlpEmptyException {

        RlpParser envelopeParser = new RlpParser(rlp);
        ByteString envelope = envelopeParser.next();

        if (!envelopeParser.atEnd()) {
            throw new EthRpcHandlerException(ErrorCodeType.RAW_TRANSCATION_UNPARSED);
        }

        RlpParser parser = new RlpParser(envelope);

        final ByteString nonceV = nextPart(parser, "nonce");
        final ByteString gasPriceV = nextPart(parser, "gas price");
        final ByteString gasV = nextPart(parser, "start gas");
        final ByteString to = nextPart(parser, "to address");
        final ByteString value = nextPart(parser, "value");
        final ByteString data = nextPart(parser, "data");
        final ByteString vV = nextPart(parser, "signature V");
        ByteString r = nextPart(parser, "signature R");
        if (r.size() > 32) {
            throw new EthRpcHandlerException(ErrorCodeType.INVALID_RAW_TRANSACTION);
        } else if (r.size() < 32) {
            // pad out to 32 bytes to make things easy for Concord
            byte[] leadingZeros = new byte[32 - r.size()];
            r = ByteString.copyFrom(leadingZeros).concat(r);
        }
        ByteString s = nextPart(parser, "signature S");
        if (s.size() > 32) {
            throw new EthRpcHandlerException(ErrorCodeType.INVALID_RAW_TRANSACTION);
        } else if (s.size() < 32) {
            // pad out to 32 bytes to make things easy for Concord
            byte[] leadingZeros = new byte[32 - s.size()];
            s = ByteString.copyFrom(leadingZeros).concat(s);
        }

        if (!parser.atEnd()) {
            throw new EthRpcHandlerException(ErrorCodeType.RAW_TRANSCATION_UNPARSED);
        }

        if (to.size() != 0 && to.size() != 20) {
            throw new EthRpcHandlerException(ErrorCodeType.INVALID_RAW_TRANSACTION);
        }


        final long nonce = ApiHelper.bytesToLong(nonceV);
        final long gasPrice = ApiHelper.bytesToLong(gasPriceV);
        final long gas = ApiHelper.bytesToLong(gasV);
        final long v = ApiHelper.bytesToLong(vV);

        b.setNonce(nonce);
        b.setGasPrice(gasPrice);
        b.setGas(gas);
        if (to.size() > 0) {
            b.setAddrTo(to);
        }
        b.setValue(value);
        b.setData(data);
        b.setSigV(v);
        b.setSigR(r);
        b.setSigS(s);
    }

    private ByteString nextPart(RlpParser parser, String label) throws EthRpcHandlerException {
        try {
            ByteString b = parser.next();
            logger.trace("Extracted " + label + ": " + b.size() + " bytes");
            return b;
        } catch (RlpParser.RlpEmptyException e) {
            throw new EthRpcHandlerException(ErrorCodeType.RAW_TRANSCATION_UNPARSED);
        }
    }

    /**
     * Builds the response object to be returned to the user.
     *
     * @param concordResponse Response received from Concord
     * @param requestJson Request parameters passed by the user
     * @return response to be returned to the user
     */
    @SuppressWarnings("unchecked")
    @Override
    public JSONObject buildResponse(Concord.ConcordResponse concordResponse, JSONObject requestJson) {
        EthResponse ethResponse = concordResponse.getEthResponse(0);
        JSONObject respObject = initializeResponseObject(ethResponse);
        // Set method specific responses
        String method = (String) requestJson.get("method");
        String fromParam = "";
        String toParam = "";
        String byteCode = "";
        // params will only be an object when using eth_sendTransaction. To avoid parsing errors,
        // only perform the following steps if this is the method being used
        boolean isSendTransaction = method.equals(Constants.SEND_TRANSACTION_NAME);
        if (isSendTransaction) {
            JSONArray paramsArray = (JSONArray) requestJson.get("params");
            JSONObject params = (JSONObject) paramsArray.get(0);
            fromParam = (String) params.get("from");
            toParam = (String) params.get("to");
            byteCode = (String) params.get("data");
            if (byteCode != null) {
                byteCode = byteCode.replace("0x", "");
            }
        }
        respObject.put("result", ApiHelper.binaryStringToHex(ethResponse.getData()));
        if (isSendTransaction && !isInternalContract && toParam == null) {
            try {
                handleSmartContractCreation(ApiHelper.binaryStringToHex(ethResponse.getData()), fromParam, byteCode);
            } catch (Exception e) {
                logger.error("Error in smart contract linking.", e);
            }
        }
        return respObject;
    }

    private void handleSmartContractCreation(String transactionHash, String from, String byteCode) throws Exception {
        final JSONObject ethRequest = new JSONObject();
        final JSONArray paramsArray = new JSONArray();
        ethRequest.put("id", 1);
        ethRequest.put("jsonrpc", jsonRpc);
        ethRequest.put("method", "eth_getTransactionReceipt");
        paramsArray.add(transactionHash);
        ethRequest.put("params", paramsArray);
        String responseString =
                new EthDispatcher(registryManager, connectionPoolManager, profilesRegistryManager, defaultProfiles,
                                  authHelper)
                        .dispatch(blockchain, ethRequest).toJSONString();
        try {
            JSONObject txReceipt = (JSONObject) new JSONParser().parse(responseString);
            JSONObject result = (JSONObject) txReceipt.get("result");
            if (result.get("contractAddress") != null) {
                String contractVersion = "1";
                String contractAddress = (String) result.get("contractAddress");
                String metaData = "";
                String solidityCode = "";
                UUID bid = blockchain.orElse(defaultProfiles.getBlockchain().getId());
                registryManager.addNewContractVersion(contractAddress, from, contractVersion,
                        contractAddress, metaData, byteCode, solidityCode, bid);
            }
        } catch (Exception e) {
            logger.error("Error parsing transaction receipt response", e);
        }

    }
}

package services.EthRPCHandlers;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.json.simple.JSONArray;
import org.json.simple.JSONObject;
import org.json.simple.parser.JSONParser;

import com.google.protobuf.ByteString;
import com.vmware.athena.Athena;
import com.vmware.athena.Athena.EthRequest;
import com.vmware.athena.Athena.EthRequest.EthMethod;
import com.vmware.athena.Athena.EthResponse;

import Servlets.APIHelper;
import Servlets.EthDispatcher;

import services.contracts.ContractRegistryManager;

/**
 * <p>
 * Copyright 2018 VMware, all rights reserved.
 * </p>
 *
 * <p>
 * This handler is used to service eth_sendTransaction and eth_call POST
 * requests. These are bundled together here because functionally, the
 * processing for both these request types is similar.
 * </p>
 */
public class EthSendTxHandler extends AbstractEthRPCHandler {

   private static Logger logger = LogManager.getLogger(EthSendTxHandler.class);
   private static boolean isInternalContract;
   private ContractRegistryManager registryManager;

   public EthSendTxHandler(boolean _isInternalContract) {
      // If isInternalContract is true, the handler is processing a contract created from the Helen UI.
      isInternalContract = _isInternalContract;
      try {
         registryManager = ContractRegistryManager.getInstance();
      } catch (Exception e) {
         logger.fatal("Unable to instantiate ContractRegistryManager", e);
         registryManager = null;
      }
   }

   /**
    * Builds the Athena request builder. Extracts the method name, from, to,
    * data and value fields from the request and uses it to set up an Athena
    * Request builder with an EthRequest.
    *
    * 'from' is mandatory for send tx and 'to' is mandatory for call contract.
    *
    * @param builder
    *           Object in which request is built
    * @param requestJson
    *           Request parameters passed by the user
    */
   @Override
   public void
          buildRequest(Athena.AthenaRequest.Builder athenaRequestBuilder,
                       JSONObject requestJson) throws EthRPCHandlerException,
                                               APIHelper.HexParseException,
                                               RLPParser.RLPEmptyException {

      String from = null, to = null, data = null, value = null;
      Athena.EthRequest ethRequest = null;
      EthRequest.Builder b = initializeRequestObject(requestJson);
      String method = EthDispatcher.getEthMethodName(requestJson);

      JSONArray params = extractRequestParams(requestJson);
      if (method.equals(_conf.getStringValue("SendTransaction_Name"))) {
         b.setMethod(EthMethod.SEND_TX);
         buildRequestFromObject(b,
                                (JSONObject) params.get(0),
                                true /* isSendTx */);
      } else if (method.equals(_conf.getStringValue("SendRawTransaction_Name"))) {
         b.setMethod(EthMethod.SEND_TX);
         buildRequestFromString(b, (String) params.get(0));
      } else {
         b.setMethod(EthMethod.CALL_CONTRACT);
         buildRequestFromObject(b,
                                (JSONObject) params.get(0),
                                false /* isSendTx */);
         // add "block" parameter
         if (params.size() == 2) {
            long blockNumber = APIHelper.parseBlockNumber(params);
            if (blockNumber != -1){
               b.setBlockNumber(blockNumber);
            }
         }
      }

      ethRequest = b.build();
      athenaRequestBuilder.addEthRequest(ethRequest);
   }

   private void
           buildRequestFromObject(EthRequest.Builder b, JSONObject obj,
                                  boolean isSendTx) throws EthRPCHandlerException,
                                                    APIHelper.HexParseException {

      if (obj.containsKey("from")) {
         String from = (String) obj.get("from");
         ByteString fromAddr = APIHelper.hexStringToBinary(from);
         b.setAddrFrom(fromAddr);
      } else if (isSendTx) {
         // TODO: if we allow r/s/v signature fields, we don't have to require
         // 'from' when they're present
         logger.error("From field missing in params");
         throw new EthRPCHandlerException("'from' must be specified");
      }

      if (obj.containsKey("to")) {
         String to = (String) obj.get("to");
         ByteString toAddr = APIHelper.hexStringToBinary(to);
         b.setAddrTo(toAddr);
      } else if (!isSendTx) {
         logger.error("To field missing in params");
         throw new EthRPCHandlerException("'to' must be specified");
      }

      if (obj.containsKey("data")) {
         String data = (String) obj.get("data");
         if (data != null) {
            ByteString dataBytes = APIHelper.hexStringToBinary(data);
            b.setData(dataBytes);
         }
      }

      if (obj.containsKey("value")) {
         String value = (String) obj.get("value");
         if (value != null) {
            ByteString valueBytes
               = APIHelper.hexStringToBinary(APIHelper.padZeroes(value));
            b.setValue(valueBytes);
         }
      }

      // TODO: add gas, gasPrice, nonce, r, s, v
      // (no, rsv are not specified in the doc, but why not?)
   }

   private void
           buildRequestFromString(EthRequest.Builder b,
                                  String rlp) throws EthRPCHandlerException,
                                              APIHelper.HexParseException,
                                              RLPParser.RLPEmptyException {

      RLPParser envelope_parser = new RLPParser(rlp);
      ByteString envelope = envelope_parser.next();

      if (!envelope_parser.atEnd()) {
         throw new EthRPCHandlerException("Unable to parse raw transaction (extra data after envelope)");
      }

      RLPParser parser = new RLPParser(envelope);

      ByteString nonce_v = nextPart(parser, "nonce");
      ByteString gasPrice_v = nextPart(parser, "gas price");
      ByteString gas_v = nextPart(parser, "start gas");
      ByteString to = nextPart(parser, "to address");
      ByteString value = nextPart(parser, "value");
      ByteString data = nextPart(parser, "data");
      ByteString v_v = nextPart(parser, "signature V");
      ByteString r = nextPart(parser, "signature R");
      ByteString s = nextPart(parser, "signature S");

      if (!parser.atEnd()) {
         throw new EthRPCHandlerException("Unable to parse raw transaction (extra data in envelope)");
      }

      long nonce = APIHelper.bytesToLong(nonce_v);
      long gasPrice = APIHelper.bytesToLong(gasPrice_v);
      long gas = APIHelper.bytesToLong(gas_v);
      long v = APIHelper.bytesToLong(v_v);

      if (to.size() != 0 && to.size() != 20) {
         throw new EthRPCHandlerException("Invalid raw transaction (to address too short)");
      }

      if (r.size() > 32) {
         throw new EthRPCHandlerException(
            "Invalid raw transaction (signature R too large)");
      } else if (r.size() < 32) {
         // pad out to 32 bytes to make things easy for Athena
         byte[] leadingZeros = new byte[32-r.size()];
         r = ByteString.copyFrom(leadingZeros).concat(r);
      }

      if (s.size() > 32) {
         throw new EthRPCHandlerException(
            "Invalid raw transaction (signature S too large)");
      } else if (s.size() < 32) {
         // pad out to 32 bytes to make things easy for Athena
         byte[] leadingZeros = new byte[32-s.size()];
         s = ByteString.copyFrom(leadingZeros).concat(s);
      }

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

   private ByteString nextPart(RLPParser parser,
                               String label) throws EthRPCHandlerException {
      try {
         ByteString b = parser.next();
         logger.trace("Extracted " + label + ": " + b.size() + " bytes");
         return b;
      } catch (RLPParser.RLPEmptyException e) {
         throw new EthRPCHandlerException("Unable to decode " + label
            + " from raw transaction");
      }
   }

   /**
    * Builds the response object to be returned to the user.
    *
    * @param athenaResponse
    *           Response received from Athena
    * @param requestJson
    *           Request parameters passed by the user
    * @return response to be returned to the user
    */
   @SuppressWarnings("unchecked")
   @Override
   public JSONObject buildResponse(Athena.AthenaResponse athenaResponse,
                                   JSONObject requestJson) {
      EthResponse ethResponse = athenaResponse.getEthResponse(0);
      JSONObject respObject = initializeResponseObject(ethResponse);
      // Set method specific responses
      String method = (String) requestJson.get("method");
      String fromParam = "";
      String toParam = "";
      String byteCode = "";
      // params will only be an object when using eth_sendTransaction. To avoid parsing errors,
      // only perform the following steps if this is the method being used
      boolean isSendTransaction = method.equals(_conf.getStringValue("SendTransaction_Name"));
      if(isSendTransaction) {
         JSONArray paramsArray = (JSONArray) requestJson.get("params");
         JSONObject params = (JSONObject) paramsArray.get(0);
         fromParam = (String) params.get("from");
         toParam = (String) params.get("to");
         byteCode = (String) params.get("data");
         if(byteCode != null) {
            byteCode = byteCode.replace("0x", "");
         }
      }
      respObject.put("result",
                     APIHelper.binaryStringToHex(ethResponse.getData()));
      if (isSendTransaction && !isInternalContract && toParam == null) {
         try {
            handleSmartContractCreation(APIHelper.binaryStringToHex(ethResponse.getData()), fromParam, byteCode);
         } catch (Exception e) {
            logger.error("Error in smart contract linking.", e);
         }
      }
      return respObject;
   }

   private void handleSmartContractCreation(String transactionHash, String from, String byteCode) throws Exception  {
      JSONObject ethRequest = new JSONObject();
      JSONArray paramsArray = new JSONArray();
      ethRequest.put("id", 1);
      ethRequest.put("jsonrpc", jsonRpc);
      ethRequest.put("method", "eth_getTransactionReceipt");
      paramsArray.add(transactionHash);
      ethRequest.put("params", paramsArray);
      String responseString
              = new EthDispatcher().dispatch(ethRequest).toJSONString();
      try {
         JSONObject txReceipt
                 = (JSONObject) new JSONParser().parse(responseString);
         JSONObject result = (JSONObject) txReceipt.get("result");
         if(result.get("contractAddress") != null) {
            String contractVersion = "1";
            String contractAddress = (String) result.get("contractAddress");
            String metaData = "";
            String solidityCode = "";
            boolean success = registryManager.addNewContractVersion(contractAddress,
                    from,
                    contractVersion,
                    contractAddress,
                    metaData,
                    byteCode,
                    solidityCode);
         }
      } catch (Exception e) {
         logger.error("Error parsing transaction receipt response", e);
      }

   }
}

package services.EthRPCHandlers;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.json.simple.JSONArray;
import org.json.simple.JSONObject;

import com.google.protobuf.ByteString;
import com.vmware.athena.Athena;

import Servlets.APIHelper;
import Servlets.EthDispatcher;

/**
 * <p>
 * Copyright 2018 VMware, all rights reserved.
 * </p>
 *
 * <p>
 * This Handler is used for handling all `eth_getTransactionReceipt` types of
 * requests. EthGetTxReceiptHandler is little different than other handlers
 * because It leverages already existing `TransactionReceipt` AthenaRequest to
 * handle `eth_getTransactionReceipt` requests (see Transaction.java file which
 * implements this API). Hence, in this handler we actually put a
 * `TransactionRequest` inside AthenaRequest and read a TransactionResponse from
 * AthenaResponse.
 * </p>
 */
public class EthGetTxReceiptHandler extends AbstractEthRPCHandler {

   Logger logger = LogManager.getLogger(EthGetTxReceiptHandler.class);

   /**
    * Builds a TransactionRequest object from given requestJson and inserts it
    * into AthenaRequest Object.
    *
    * @param builder
    *           Athena Request Builder.
    * @param requestJson
    *           The JSONObject of original RPC request.
    * @throws Exception
    */
   public void buildRequest(Athena.AthenaRequest.Builder builder,
                            JSONObject requestJson) throws Exception {
      try {
         logger.debug("Inside GetTXReceipt buildRequest");
         // Construct a transaction request object.
         JSONArray params = extractRequestParams(requestJson);
         String txHash = (String) params.get(0);
         ByteString hashBytes = APIHelper.hexStringToBinary(txHash);

         Athena.TransactionRequest txRequestObj
            = Athena.TransactionRequest.newBuilder().setHash(hashBytes).build();
         builder.setTransactionRequest(txRequestObj);
      } catch (Exception e) {
         logger.error("Exception in tx receipt handler", e);
         throw e;
      }
   }

   /**
    * Since the parents initializeResponseObject method takes EthResponse object
    * as input we override it here to take in the id directly.
    *
    * @param id
    * @return
    */
   @SuppressWarnings("unchecked")
   JSONObject initializeResponseObject(long id) {
      JSONObject respObject = new JSONObject();
      respObject.put("id", id);
      respObject.put("jsonrpc", jsonRpc);
      return respObject;
   }

   /**
    * Builds a response JSON object by extracting TransactionResponse object
    * from given AthenaResponse Object.
    *
    * @param athenaResponse
    *           The AthenaResponse object
    * @param requestJson
    *           The json object of original RPC request.
    * @return the response JSONObject.
    */
   @SuppressWarnings("unchecked")
   @Override
   public JSONObject buildResponse(Athena.AthenaResponse athenaResponse,
                                   JSONObject requestJson) {

      JSONObject respObject = new JSONObject();
      try {
         Athena.TransactionResponse transactionResponse
            = athenaResponse.getTransactionResponse();

         respObject
            = initializeResponseObject(EthDispatcher.getEthRequestId(requestJson));

         JSONObject result = new JSONObject();

         result.put("transactionHash",
                    APIHelper.binaryStringToHex(transactionResponse.getHash()));
         result.put("transactionIndex",
                    transactionResponse.getTransactionIndex());
         result.put("blockNumber", transactionResponse.getBlockNumber());
         result.put("blockHash",
                    APIHelper.binaryStringToHex(transactionResponse.getBlockHash()));
         if (transactionResponse.hasContractAddress()) {
            result.put("contractAddress",
                       APIHelper.binaryStringToHex(transactionResponse.getContractAddress()));
         } else {
            result.put("contractAddress", null);
         }

         // Athena EVM has status code '0' for success and other Positive
         // values to denote error. However, for JSON RPC '1' is success
         // and '0' is failure. Here we need to reverse status value of athena
         // response before returning it.
         result.put("status",
                    "0x" + Integer.toString(transactionResponse.getStatus() == 0
                       ? 1 : 0));

         // TODO : Passing empty String array for logs as Truffle expects this
         //        Fix this with actual logs - HEL 128
         //JSONObject logs = new JSONObject();
         //logs.put("blockHash", APIHelper.binaryStringToHex(transactionResponse.getBlockHash()));
         //String[] logs = new String[0];
         JSONArray logs = new JSONArray();
         result.put("logs", logs);
         respObject.put("result", result);
      } catch (Exception e) {
         // This should never get triggered as params are already checked while
         // building the request
         logger.fatal("'params' not present");
      }
      return respObject;
   }
}

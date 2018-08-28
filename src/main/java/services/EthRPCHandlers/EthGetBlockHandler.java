package services.EthRPCHandlers;

import java.util.List;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.json.simple.JSONArray;
import org.json.simple.JSONObject;

import com.google.protobuf.ByteString;
import com.vmware.athena.Athena;
import com.vmware.athena.Athena.AthenaRequest.Builder;
import com.vmware.athena.Athena.AthenaResponse;
import com.vmware.athena.Athena.BlockResponse;
import com.vmware.athena.Athena.TransactionResponse;

import Servlets.APIHelper;
import Servlets.EthDispatcher;

/**
 * <p>
 * Copyright 2018 VMware, all rights reserved.
 * </p>
 *
 * This handler is used to service eth_getBlockByHash POST requests.
 */
public class EthGetBlockHandler extends AbstractEthRPCHandler {

   Logger logger = LogManager.getLogger(EthGetBlockHandler.class);

   /**
    * Builds the Athena request builder. Extracts the block hash from the
    * request and uses it to set up an Athena Request builder with a
    * BlockRequest. Also performs basic checks on parameters.
    *
    * @param builder
    *           Object in which request is built
    * @param requestJson
    *           Request parameters passed by the user
    */
   @Override
   public void buildRequest(Builder builder,
                            JSONObject requestJson) throws Exception {
      try {
         JSONArray params = extractRequestParams(requestJson);
         if (params.size() != 2) {
            throw new Exception("Params should contain 2 elements for this "
               + "request type");
         }

         String ethMethodName = EthDispatcher.getEthMethodName(requestJson);

         // Perform type checking of the flag at this stage itself rather than
         // while building the response
         @SuppressWarnings("unused")
         boolean flag = (boolean) params.get(1);

         // Construct a blockNumberRequest object. Set its start field.
         final Athena.BlockRequest blockRequestObj;

         if (ethMethodName.equals(_conf.getStringValue("GetBlockByNumber_Name"))) {
            // Block number string can be either a hex number or it can be one
            // of "latest", "earliest", "pending". Since, athena only accepts
            // uint64_t for block number we will replace "latest" with -1
            // "earliest" with 0 (genesis block) and "pending" with -1 (since
            // in athena blocks are generated instantaneously we can say that
            // "latest" = "pending"
            String requestedBlockStr = (String) params.get(0);
            long requestedBlockNumber = -1;
            if (requestedBlockStr.equals("earliest")) {
               requestedBlockNumber = 0;
            } else if (requestedBlockStr.equals("latest")
               || requestedBlockStr.equals("pending")) {
               requestedBlockNumber = -1;
            } else if (requestedBlockStr.startsWith("0x")) {
               requestedBlockNumber
                  = Long.parseLong(requestedBlockStr.substring(2), 16);
            } else {
               throw new Exception("Invalid block number requested. Block "
                  + "number can either be 'latest', 'pending', 'earliest',"
                  + " or a hex number starting with '0x'");
            }
            blockRequestObj
               = Athena.BlockRequest.newBuilder()
                                    .setNumber(requestedBlockNumber)
                                    .build();
         } else { // BlockByHash_Name
            ByteString blockHash
               = APIHelper.hexStringToBinary((String) params.get(0));
            blockRequestObj
               = Athena.BlockRequest.newBuilder().setHash(blockHash).build();
         }

         // Add the request to the athena request builder
         builder.setBlockRequest(blockRequestObj);
      } catch (Exception e) {
         logger.error("Exception in get block handler", e);
         throw e;
      }
   }

   /**
    * Builds the response object to be returned to the user. Checks the flag in
    * the request to determine whether a list of transaction hashes or a list of
    * transaction objects needs to be returned to the user.
    *
    * @param athenaResponse
    *           Response received from Athena
    * @param requestJson
    *           Request parameters passed by the user
    * @return response to be returned to the user
    */
   @SuppressWarnings("unchecked")
   @Override
   public JSONObject buildResponse(AthenaResponse athenaResponse,
                                   JSONObject requestJson) throws Exception {
      BlockResponse blockResponseObj = athenaResponse.getBlockResponse();
      long id = EthDispatcher.getEthRequestId(requestJson);

      JSONObject response = new JSONObject();
      response.put("id", id);
      response.put("jsonrpc", jsonRpc);

      JSONObject result = new JSONObject();
      result.put("number", blockResponseObj.getNumber());
      result.put("hash",
                 APIHelper.binaryStringToHex(blockResponseObj.getHash()));
      result.put("parentHash",
                 APIHelper.binaryStringToHex(blockResponseObj.getParentHash()));
      result.put("nonce",
                 APIHelper.binaryStringToHex(blockResponseObj.getNonce()));
      result.put("size", blockResponseObj.getSize());

      if (blockResponseObj.hasAcceptedTimestamp()) {
         /**
          * We use "accepted" rather than "proposed" here, because that time is
          * guaranteed not to go backward. Ethereum clients expect that time
          * will not reverse.
          */
         result.put("timestamp",
                    "0x" + Long.toHexString(
                       blockResponseObj.getAcceptedTimestamp()));
      }

      JSONArray transactions = new JSONArray();
      JSONArray params = extractRequestParams(requestJson);
      boolean flag = (boolean) params.get(1);

      List<TransactionResponse> list
         = (List<TransactionResponse>) blockResponseObj.getTransactionList();

      // include all details about transaction
      if (flag) {
         for (TransactionResponse tr : list) {
            JSONObject transaction = new JSONObject();
            transaction.put("hash", APIHelper.binaryStringToHex(tr.getHash()));
            transaction.put("nonce", "0x" + Long.toHexString(tr.getNonce()));
            transaction.put("blockHash",
                            APIHelper.binaryStringToHex(tr.getBlockHash()));
            transaction.put("blockNumber",
                            "0x" + Long.toHexString(tr.getBlockNumber()));
            transaction.put("transactionIndex",
                            "0x" + Long.toHexString(tr.getTransactionIndex()));
            transaction.put("from", APIHelper.binaryStringToHex(tr.getFrom()));
            transaction.put("to", APIHelper.binaryStringToHex(tr.getTo()));
            transaction.put("value", "0x" + Long.toString(tr.getValue()));
            transaction.put("input",
                            APIHelper.binaryStringToHex(tr.getInput()));
            transaction.put("contractAddress",
                            APIHelper.binaryStringToHex(tr.getContractAddress()));
            transactions.add(transaction);
         }
      }
      // only include the transaction hashes
      else {
         for (TransactionResponse tr : list) {
            transactions.add(APIHelper.binaryStringToHex(tr.getHash()));
         }
      }
      result.put("transactions", transactions);
      response.put("result", result);

      return response;
   }
}

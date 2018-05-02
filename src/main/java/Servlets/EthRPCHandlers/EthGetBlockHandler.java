/**
 * This handler is used to service eth_getBlockByHash POST requests.
 */
package Servlets.EthRPCHandlers;

import java.util.List;

import org.apache.log4j.Logger;
import org.json.simple.JSONArray;
import org.json.simple.JSONObject;

import com.google.protobuf.ByteString;
import com.vmware.athena.Athena.AthenaRequest.Builder;

import Servlets.APIHelper;
import Servlets.EthDispatcher;

import com.vmware.athena.Athena;
import com.vmware.athena.Athena.AthenaResponse;
import com.vmware.athena.Athena.BlockResponse;
import com.vmware.athena.Athena.TransactionResponse;

public class EthGetBlockHandler extends AbstractEthRPCHandler {

   Logger logger = Logger.getLogger(EthGetBlockHandler.class);

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

         // Perform type checking of the flag at this stage itself rather than
         // while building the reponse
         @SuppressWarnings("unused")
         boolean flag = (boolean) params.get(1);

         ByteString blockHash
            = APIHelper.hexStringToBinary((String) params.get(0));

         // Construct a blockNumberRequest object. Set its start field.
         final Athena.BlockRequest blockRequestObj
            = Athena.BlockRequest.newBuilder().setHash(blockHash).build();

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

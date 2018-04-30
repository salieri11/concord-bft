package Servlets.EthRPCHandlers;

import org.apache.log4j.Logger;
import org.json.simple.JSONArray;
import org.json.simple.JSONObject;

import com.google.protobuf.ByteString;
import com.vmware.athena.Athena;
import com.vmware.athena.Athena.EthResponse;

import Servlets.APIHelper;

/**
 * EthGetTxReceiptHandler is little different than other handlers because It
 * leverages already existing `TransactionReceipt` AthenaRequest to handle
 * `eth_getTransactionReceipt` requests. But this requires that the handler
 * understands and works with `AthenaRequest/Response` objects rather than
 * `EthRequest/Response` objects. Hence this handler provides similar
 * `buildRequest` & `buildResponse` methods but with `AthenaRequest/Response`
 * objects
 */
public class EthGetTxReceiptHandler extends AbstractEthRPCHandler {

   Logger logger = Logger.getLogger(EthGetStorageAtHandler.class);

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

   @SuppressWarnings("unchecked")
   @Override
   public JSONObject buildResponse(Athena.AthenaResponse athenaResponse,
                                   JSONObject requestJson) {
      EthResponse ethResponse = athenaResponse.getEthResponse(0);
      JSONObject respObject = initializeResponseObject(ethResponse);
      JSONObject result = new JSONObject();
      result.put("status",
                 "0x" + Integer.toString(ethResponse.getStatus(), 16));

      JSONArray params = null;
      try {
         params = extractRequestParams(requestJson);
      } catch (Exception e) {
         // This should never get triggered as params are already checked while
         // building the request
         logger.error("'params' not present");
      }
      String txHash = (String) params.get(0);

      result.put("transactionHash", txHash);
      if (ethResponse.hasContractAddress()) {
         result.put("contractAddress",
                    APIHelper.binaryStringToHex(ethResponse.getContractAddress()));
      } else {
         result.put("contractAddress", null);
      }
      respObject.put("result", result);
      return respObject;
   }
}

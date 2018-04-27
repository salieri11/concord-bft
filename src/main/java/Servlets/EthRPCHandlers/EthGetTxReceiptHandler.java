package Servlets.EthRPCHandlers;

import Servlets.APIHelper;
import com.vmware.athena.Athena;
import com.vmware.athena.Athena.EthRequest;
import com.vmware.athena.Athena.EthRequest.EthMethod;
import com.vmware.athena.Athena.EthResponse;
import org.apache.log4j.Logger;
import org.json.simple.JSONArray;
import org.json.simple.JSONObject;

public class EthGetTxReceiptHandler extends AbstractEthRPCHandler {

   Logger logger = Logger.getLogger(EthGetStorageAtHandler.class);

   @Override
   public EthRequest buildRequest(JSONObject requestJson) throws Exception {
      Athena.EthRequest ethRequest = null;
      try {
         EthRequest.Builder b = initializeRequestObject(requestJson);
         b.setMethod(EthMethod.GET_TX_RECEIPT);
         JSONArray params = extractRequestParams(requestJson);
         String txHash = (String) params.get(0);
         b.setData(APIHelper.hexStringToBinary(txHash));
         ethRequest = b.build();
      } catch (Exception e) {
         logger.error("Exception in tx receipt handler", e);
         throw e;
      }
      return ethRequest;
   }

   @SuppressWarnings("unchecked")
   @Override
   public JSONObject buildResponse(EthResponse ethResponse,
                                   JSONObject requestJson) {
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

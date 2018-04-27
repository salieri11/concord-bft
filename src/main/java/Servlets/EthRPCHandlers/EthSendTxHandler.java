package Servlets.EthRPCHandlers;

import org.apache.log4j.Logger;
import org.json.simple.JSONArray;
import org.json.simple.JSONObject;

import com.google.protobuf.ByteString;
import com.vmware.athena.Athena; 
import com.vmware.athena.Athena.EthRequest;
import com.vmware.athena.Athena.EthResponse;
import com.vmware.athena.Athena.EthRequest.EthMethod;

import Servlets.APIHelper;

public class EthSendTxHandler extends AbstractEthRPCHandler {

   Logger logger = Logger.getLogger(EthGetStorageAtHandler.class);

   @Override
   public EthRequest buildRequest(JSONObject requestJson) throws Exception {
      boolean isSendTx = false;
      String from = null, to = null, data = null, value = null;
      Athena.EthRequest ethRequest = null;

      try {
         EthRequest.Builder b = initializeRequestObject(requestJson);
         String method = extractEthMethodName(requestJson);

         if (method.equals(_conf.getStringValue("SendTransaction_Name"))) {
            b.setMethod(EthMethod.SEND_TX);
            isSendTx = true;
         } else {
            b.setMethod(EthMethod.CALL_CONTRACT);
         }

         JSONArray params = extractRequestParams(requestJson);
         if (params == null) {
            logger.error("'params' not present");
            throw new EthRPCHandlerException(buildError("'params' not present",
                                                        b.getId()));
         }

         JSONObject obj = (JSONObject) params.get(0);

         if (obj.containsKey("from")) {
            from = (String) obj.get("from");
         }
         if (isSendTx && from == null) {
            logger.error("From field missing in params");
            throw new Exception("'from' must be specified");
         }

         if (from != null) {
            ByteString fromAddr = APIHelper.hexStringToBinary(from);
            b.setAddrFrom(fromAddr);
         }

         if (obj.containsKey("to")) {
            to = (String) obj.get("to");
         }
         if (!isSendTx && to == null) {
            logger.error("To field missing in params");
            throw new Exception("'to' must be specified");
         }

         if (to != null) {
            ByteString toAddr = APIHelper.hexStringToBinary(to);
            b.setAddrTo(toAddr);
         }

         if (obj.containsKey("data")) {
            data = (String) obj.get("data");
         }

         if (data != null) {
            ByteString dataBytes = APIHelper.hexStringToBinary(data);
            b.setData(dataBytes);
         }

         if (obj.containsKey("value")) {
            value = (String) obj.get("value");
         }

         if (value != null) {
            ByteString valueBytes
               = APIHelper.hexStringToBinary(APIHelper.padZeroes(value));
            b.setValue(valueBytes);
         }
         ethRequest = b.build();
      } catch (Exception e) {
         logger.error(e.getMessage());
         throw e;
      }
      return ethRequest;
   }

   @SuppressWarnings("unchecked")
   @Override
   public JSONObject buildResponse(EthResponse ethResponse,
                                   JSONObject requestJson) {
      JSONObject respObject = initializeResponseObject(ethResponse);
      // Set method specific responses
      respObject.put("result",
                     APIHelper.binaryStringToHex(ethResponse.getData()));
      return respObject;
   }
}

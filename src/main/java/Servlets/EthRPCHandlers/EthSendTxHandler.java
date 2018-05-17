package Servlets.EthRPCHandlers;

import org.apache.log4j.Logger;
import org.json.simple.JSONArray;
import org.json.simple.JSONObject;

import com.google.protobuf.ByteString;
import com.vmware.athena.Athena;
import com.vmware.athena.Athena.EthRequest;
import com.vmware.athena.Athena.EthRequest.EthMethod;
import com.vmware.athena.Athena.EthResponse;

import Servlets.APIHelper;
import Servlets.EthDispatcher;

/**
 * <p>Copyright 2018 VMware, all rights reserved.</p>
 * 
 * <p>This handler is used to service eth_sendTransaction and eth_call POST
 * requests. These are bundled together here because functionally, the
 * processing for both these request types is similar.</p>
 */
public class EthSendTxHandler extends AbstractEthRPCHandler {

   Logger logger = Logger.getLogger(EthSendTxHandler.class);

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
   public void buildRequest(Athena.AthenaRequest.Builder athenaRequestBuilder,
                            JSONObject requestJson) throws Exception {
      boolean isSendTx = false;
      String from = null, to = null, data = null, value = null;
      Athena.EthRequest ethRequest = null;
      try {
         EthRequest.Builder b = initializeRequestObject(requestJson);
         String method = EthDispatcher.getEthMethodName(requestJson);

         long id = EthDispatcher.getEthRequestId(requestJson);

         if (method.equals(_conf.getStringValue("SendTransaction_Name"))) {
            b.setMethod(EthMethod.SEND_TX);
            isSendTx = true;
         } else {
            b.setMethod(EthMethod.CALL_CONTRACT);
         }
         JSONArray params = extractRequestParams(requestJson);

         JSONObject obj = (JSONObject) params.get(0);

         if (obj.containsKey("from")) {
            from = (String) obj.get("from");
         }
         if (isSendTx && from == null) {
            logger.error("From field missing in params");
            throw new EthRPCHandlerException(EthDispatcher.errorMessage("'from' must be specified",
                                                                        id,
                                                                        jsonRpc));
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
            throw new EthRPCHandlerException(EthDispatcher.errorMessage("'to' must be specified",
                                                                        id,
                                                                        jsonRpc));
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
         logger.error("Exception in send tx handler", e);
         throw e;
      }
      athenaRequestBuilder.addEthRequest(ethRequest);
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
      respObject.put("result",
                     APIHelper.binaryStringToHex(ethResponse.getData()));
      return respObject;
   }
}

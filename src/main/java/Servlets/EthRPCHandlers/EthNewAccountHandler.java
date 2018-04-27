package Servlets.EthRPCHandlers;

import Servlets.APIHelper;
import Servlets.EthDispatcher;
import com.google.protobuf.ByteString;
import com.vmware.athena.Athena;
import com.vmware.athena.Athena.EthRequest;
import com.vmware.athena.Athena.EthRequest.EthMethod;
import com.vmware.athena.Athena.EthResponse;
import org.apache.log4j.Logger;
import org.json.simple.JSONArray;
import org.json.simple.JSONObject;

import java.io.UnsupportedEncodingException;
import java.nio.charset.StandardCharsets;

public class EthNewAccountHandler extends AbstractEthRPCHandler {

   Logger logger = Logger.getLogger(EthGetStorageAtHandler.class);

   @Override
   public EthRequest buildRequest(JSONObject requestJson) throws Exception {
      Athena.EthRequest ethRequest = null;
      try {
         EthRequest.Builder b = initializeRequestObject(requestJson);

         b.setMethod(EthMethod.NEW_ACCOUNT);

         JSONArray params = extractRequestParams(requestJson);
         if (params == null) {
            logger.error("'params' not present");
            throw new EthRPCHandlerException(EthDispatcher.errorMessage("'params' not present",
                                                                        b.getId(),
                                                                        jsonRpc));
         }
         String passphrase = (String) params.get(0);
         try {
            b.setData(ByteString.copyFrom(passphrase,
                                          StandardCharsets.UTF_8.name()));
         } catch (UnsupportedEncodingException e) {
            logger.error("Invalid passphrase");
            throw new EthRPCHandlerException(EthDispatcher.errorMessage("Invalid passphrase",
                                                                        b.getId(),
                                                                        jsonRpc));
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
      respObject.put("result",
                     APIHelper.binaryStringToHex(ethResponse.getData()));
      return respObject;
   }
}

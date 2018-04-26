package Servlets.EthRPCHandlers;

import org.apache.log4j.Logger;
import org.json.simple.JSONArray;
import org.json.simple.JSONObject;

import com.vmware.athena.Athena;
import com.vmware.athena.Athena.EthRequest;
import com.vmware.athena.Athena.EthResponse;
import com.vmware.athena.Athena.EthRequest.EthMethod;

import Servlets.APIHelper;
import Servlets.EthDispatcher;

public class EthGetCodeHandler extends AbstractEthRPCHandler {

   Logger logger = Logger.getLogger(EthDispatcher.class);

   @Override
   public EthRequest buildRequest(JSONObject requestJson) throws Exception {
      Athena.EthRequest athenaEthRequest = null;
      try {
         EthRequest.Builder b = initializeRequestObject(requestJson);
         b.setMethod(EthMethod.GET_CODE);

         JSONArray params = extractRequestParams(requestJson);
         b.setAddrTo(APIHelper.hexStringToBinary((String) params.get(0)));
         // ignoring "block" argument for now

         athenaEthRequest = b.build();
      } catch (Exception e) {
         logger.error(e.getMessage());
         throw e;
      }
      return athenaEthRequest;
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

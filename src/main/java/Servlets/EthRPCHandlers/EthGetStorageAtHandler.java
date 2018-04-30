package Servlets.EthRPCHandlers;

import org.apache.log4j.Logger;
import org.json.simple.JSONArray;
import org.json.simple.JSONObject;

import com.vmware.athena.Athena;
import com.vmware.athena.Athena.EthRequest;
import com.vmware.athena.Athena.EthRequest.EthMethod;
import com.vmware.athena.Athena.EthResponse;

import Servlets.APIHelper;

public class EthGetStorageAtHandler extends AbstractEthRPCHandler {

   Logger logger = Logger.getLogger(EthGetStorageAtHandler.class);

   @Override
   public void buildRequest(Athena.AthenaRequest.Builder athenaRequestBuilder,
                            JSONObject requestJson) throws Exception {
      Athena.EthRequest ethRequest = null;
      try {
         EthRequest.Builder b = initializeRequestObject(requestJson);
         b.setMethod(EthMethod.GET_STORAGE_AT);
         JSONArray params = extractRequestParams(requestJson);
         b.setAddrTo(APIHelper.hexStringToBinary((String) params.get(0)));
         String p = (String) params.get(1);
         String s = APIHelper.padZeroes(p);
         b.setData(APIHelper.hexStringToBinary(s));
         ethRequest = b.build();
      } catch (Exception e) {
         logger.error("Exception in get storage at handler", e);
         throw e;
      }
      athenaRequestBuilder.addEthRequest(ethRequest);
   }

   @SuppressWarnings("unchecked")
   @Override
   public JSONObject buildResponse(Athena.AthenaResponse athenaResponse,
                                   JSONObject requestJson) {
      EthResponse ethResponse = athenaResponse.getEthResponse(0);
      JSONObject respObject = initializeResponseObject(ethResponse);
      respObject.put("result",
                     APIHelper.binaryStringToHex(ethResponse.getData()));
      return respObject;
   }
}

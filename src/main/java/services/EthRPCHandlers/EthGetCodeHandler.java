package services.EthRPCHandlers;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.json.simple.JSONArray;
import org.json.simple.JSONObject;

import com.vmware.athena.Athena;
import com.vmware.athena.Athena.EthRequest;
import com.vmware.athena.Athena.EthRequest.EthMethod;
import com.vmware.athena.Athena.EthResponse;
import utils.APIHelper;


/**
 * <p>Copyright 2018 VMware, all rights reserved.</p>
 * 
 * This handler is used to service eth_getCode POST requests.
 */
public class EthGetCodeHandler extends AbstractEthRPCHandler {
   
   Logger logger = LogManager.getLogger(EthGetCodeHandler.class);

   /**
    * Builds the Athena request builder. Extracts the 'to' address from the
    * request and uses it to set up an Athena Request builder with an
    * EthRequest.
    * 
    * @param builder
    *           Object in which request is built
    * @param requestJson
    *           Request parameters passed by the user
    */
   @Override
   public void buildRequest(Athena.AthenaRequest.Builder athenaRequestBuilder,
                            JSONObject requestJson) throws Exception {
      Athena.EthRequest athenaEthRequest = null;
      try {
         EthRequest.Builder b = initializeRequestObject(requestJson);
         b.setMethod(EthMethod.GET_CODE);
         JSONArray params = extractRequestParams(requestJson);
         b.setAddrTo(APIHelper.hexStringToBinary((String) params.get(0)));
         // ignoring "block" argument for now

         athenaEthRequest = b.build();
      } catch (Exception e) {
         logger.error("Exception in get code handler", e);
         throw e;
      }
      athenaRequestBuilder.addEthRequest(athenaEthRequest);
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
      respObject.put("result",
                     APIHelper.binaryStringToHex(ethResponse.getData()));
      return respObject;
   }
}

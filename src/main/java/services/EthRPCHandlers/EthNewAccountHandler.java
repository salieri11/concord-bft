package services.EthRPCHandlers;

import java.io.UnsupportedEncodingException;
import java.nio.charset.StandardCharsets;

import controllers.EthDispatcher;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.json.simple.JSONArray;
import org.json.simple.JSONObject;

import com.google.protobuf.ByteString;
import com.vmware.athena.Athena;
import com.vmware.athena.Athena.EthRequest;
import com.vmware.athena.Athena.EthRequest.EthMethod;
import com.vmware.athena.Athena.EthResponse;
import utils.APIHelper;


/**
 * <p>Copyright 2018 VMware, all rights reserved.</p>
 * 
 * This handler is used to service personal_newAccount POST requests.
 */
public class EthNewAccountHandler extends AbstractEthRPCHandler {

   private static Logger logger = LogManager.getRootLogger();

   /**
    * Builds the Athena request builder. Extracts the passphrase from the
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
      Athena.EthRequest ethRequest;
      try {
         EthRequest.Builder b = initializeRequestObject(requestJson);
         b.setMethod(EthMethod.NEW_ACCOUNT);
         JSONArray params = extractRequestParams(requestJson);

         String passphrase = (String) params.get(0);
         try {
            b.setData(ByteString.copyFrom(passphrase,
                                          StandardCharsets.UTF_8.name()));
         } catch (UnsupportedEncodingException e) {
            logger.error("Invalid passphrase");
            throw new EthRPCHandlerException(
                    EthDispatcher.errorMessage("Invalid passphrase",
                            b.getId(), jsonRpc).toJSONString());
         }
         ethRequest = b.build();
      } catch (Exception e) {
         logger.error("Exception in new account handler", e);
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
      respObject.put("result",
                     APIHelper.binaryStringToHex(ethResponse.getData()));
      return respObject;
   }
}

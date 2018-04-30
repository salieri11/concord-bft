package Servlets.EthRPCHandlers;

import org.json.simple.JSONArray;
import org.json.simple.JSONObject;

import com.vmware.athena.Athena;
import com.vmware.athena.Athena.EthRequest;
import com.vmware.athena.Athena.EthResponse;

import Servlets.EthDispatcher;
import configurations.ConfigurationFactory;
import configurations.ConfigurationFactory.ConfigurationType;
import configurations.IConfiguration;

public abstract class AbstractEthRPCHandler {

   IConfiguration _conf
      = ConfigurationFactory.getConfiguration(ConfigurationType.File);

   protected String jsonRpc = _conf.getStringValue("JSONRPC");

   abstract public void buildRequest(Athena.AthenaRequest.Builder builder,
                                     JSONObject requestJson) throws Exception;

   abstract public JSONObject
            buildResponse(Athena.AthenaResponse athenaResponse,
                          JSONObject requestJson) throws Exception;

   EthRequest.Builder
             initializeRequestObject(JSONObject requestJson) throws Exception {
      EthRequest.Builder b = Athena.EthRequest.newBuilder();
      long id = EthDispatcher.getEthRequestId(requestJson);
      b.setId(id);
      return b;
   }

   JSONArray extractRequestParams(JSONObject requestJson) throws Exception {
      JSONArray params;
      try {
         params = (JSONArray) requestJson.get("params");
         if (params == null) {
            throw new EthRPCHandlerException(EthDispatcher.errorMessage("'params' not present",
                                                                        -1L,
                                                                        jsonRpc));
         }
      } catch (ClassCastException cse) {
         throw new EthRPCHandlerException(EthDispatcher.errorMessage("'params' must be an array",
                                                                     -1L,
                                                                     jsonRpc));
      } catch (Exception e) {
         throw new EthRPCHandlerException(EthDispatcher.errorMessage(e.getMessage(),
                                                                     -1L,
                                                                     jsonRpc));
      }
      return params;
   }

   @SuppressWarnings("unchecked")
   JSONObject initializeResponseObject(EthResponse ethResponse) {
      JSONObject respObject = new JSONObject();
      respObject.put("id", ethResponse.getId());
      respObject.put("jsonrpc", jsonRpc);
      return respObject;
   }

}

package Servlets.EthRPCHandlers;

import com.vmware.athena.Athena;
import com.vmware.athena.Athena.EthRequest;
import com.vmware.athena.Athena.EthResponse;

import configurations.ConfigurationFactory;
import configurations.ConfigurationFactory.ConfigurationType;
import configurations.IConfiguration;

import org.json.simple.JSONArray;
import org.json.simple.JSONObject;

public abstract class AbstractEthRPCHandler {

   IConfiguration _conf
      = ConfigurationFactory.getConfiguration(ConfigurationType.File);

   abstract public EthRequest
            buildRequest(JSONObject requestJson) throws Exception;

   abstract public JSONObject buildResponse(EthResponse ethResponse,
                                            JSONObject requestJson) throws Exception;

   String extractEthMethodName(JSONObject requestJson) {
       return (String) requestJson.get("method");
   }

   EthRequest.Builder
             initializeRequestObject(JSONObject requestJson) throws Exception {
      EthRequest.Builder b = Athena.EthRequest.newBuilder();
      long id;

      try {
         id = (long) requestJson.get("id");
      } catch (Exception e) {
         throw new EthRPCHandlerException(buildError("'id' must be an integer",
                                                     -1L));
      }
      b.setId(id);
      return b;
   }

   JSONArray extractRequestParams(JSONObject requestJson) throws Exception {
      JSONArray params;
      try {
         params = (JSONArray) requestJson.get("params");
      } catch (ClassCastException cse) {
         throw new EthRPCHandlerException(buildError("'params' must be an array",
                                                     -1L));
      }
      return params;
   }

   @SuppressWarnings("unchecked")
   JSONObject initializeResponseObject(EthResponse ethResponse) {
      JSONObject respObject = new JSONObject();
      respObject.put("id", ethResponse.getId());
      respObject.put("jsonrpc", _conf.getStringValue("JSONRPC"));
      return respObject;
   }

   @SuppressWarnings("unchecked")
   String buildError(String message, long id) {
      JSONObject responseJson = new JSONObject();
      responseJson.put("id", id);
      responseJson.put("jsonprc", _conf.getStringValue("JSONRPC"));

      JSONObject error = new JSONObject();
      error.put("message", message);
      responseJson.put("error", error);
      return responseJson.toJSONString();
   }
}

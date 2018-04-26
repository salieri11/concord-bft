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

   abstract public EthRequest
            buildRequest(JSONObject requestJson) throws Exception;

   abstract public JSONObject buildResponse(EthResponse ethResponse,
                                            JSONObject requestJson);

   EthRequest.Builder
             initializeRequestObject(JSONObject requestJson) throws Exception {
      EthRequest.Builder b = Athena.EthRequest.newBuilder();
      long id;

      try {
         id = (long) requestJson.get("id");
      } catch (Exception e) {
         IConfiguration _conf
            = ConfigurationFactory.getConfiguration(ConfigurationType.File);
         String jsonRpc = _conf.getStringValue("JSONRPC");
         throw new EthRPCHandlerException(buildError("'id' must be an integer",
                                                     -1L,
                                                     jsonRpc));
      }
      b.setId(id);
      return b;
   }

   JSONArray extractRequestParams(JSONObject requestJson) throws Exception {
      JSONArray params;
      try {
         params = (JSONArray) requestJson.get("params");
      } catch (ClassCastException cse) {
         IConfiguration _conf
            = ConfigurationFactory.getConfiguration(ConfigurationType.File);
         String jsonRpc = _conf.getStringValue("JSONRPC");
         throw new EthRPCHandlerException(buildError("'params' must be an array",
                                                     -1L,
                                                     jsonRpc));
      }
      return params;
   }

   @SuppressWarnings("unchecked")
   JSONObject initializeResponseObject(EthResponse ethResponse) {
      IConfiguration _conf
         = ConfigurationFactory.getConfiguration(ConfigurationType.File);
      JSONObject respObject = new JSONObject();
      respObject.put("id", ethResponse.getId());
      respObject.put("jsonrpc", _conf.getStringValue("JSONRPC"));
      return respObject;
   }

   @SuppressWarnings("unchecked")
   String buildError(String message, long id, String jsonRpc) {
      JSONObject responseJson = new JSONObject();
      responseJson.put("id", id);
      responseJson.put("jsonprc", jsonRpc);

      JSONObject error = new JSONObject();
      error.put("message", message);
      responseJson.put("error", error);
      return responseJson.toJSONString();
   }
}

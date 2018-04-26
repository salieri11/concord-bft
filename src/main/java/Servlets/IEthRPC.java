package Servlets;

import com.vmware.athena.Athena;
import com.vmware.athena.Athena.EthRequest;
import com.vmware.athena.Athena.EthResponse;

import configurations.ConfigurationFactory;
import configurations.ConfigurationFactory.ConfigurationType;
import configurations.IConfiguration;

import org.json.simple.JSONArray;
import org.json.simple.JSONObject;

public interface IEthRPC {

   EthRequest buildRequest(JSONObject requestJson) throws Exception;

   JSONObject buildResponse(EthResponse ethResponse, JSONObject requestJson);

   static EthRequest.Builder
          initializeRequestObject(JSONObject requestJson) throws Exception {
      EthRequest.Builder b = Athena.EthRequest.newBuilder();
      Long id;
      try {
         id = (Long) requestJson.get("id");
      } catch (NumberFormatException e) {
         throw new Exception("'id' must be an integer");
      }
      b.setId(id);
      return b;
   }

   static JSONArray
          extractRequestParams(JSONObject requestJson) throws Exception {
      JSONArray params;
      try {
         params = (JSONArray) requestJson.get("params");
      } catch (ClassCastException cse) {
         throw new Exception("'params' must be an array");
      }
      return params;
   }

   @SuppressWarnings("unchecked")
   static JSONObject initializeResponseObject(EthResponse ethResponse) {
      IConfiguration _conf
         = ConfigurationFactory.getConfiguration(ConfigurationType.File);

      JSONObject respObject = new JSONObject();
      respObject.put("id", ethResponse.getId());
      respObject.put("jsonrpc", _conf.getStringValue("JSONRPC"));
      return respObject;
   }
}

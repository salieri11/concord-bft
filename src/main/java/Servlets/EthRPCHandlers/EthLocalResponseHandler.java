package Servlets.EthRPCHandlers;

import org.json.simple.JSONArray;
import org.json.simple.JSONObject;

import com.vmware.athena.Athena.AthenaRequest;
import com.vmware.athena.Athena.AthenaResponse;

import configurations.ConfigurationFactory;
import configurations.IConfiguration;
import configurations.ConfigurationFactory.ConfigurationType;

public class EthLocalResponseHandler implements AbstractEthRPCHandler {
   private String jsonRpc;
   private IConfiguration _conf;

   public EthLocalResponseHandler() {
      _conf = ConfigurationFactory.getConfiguration(ConfigurationType.File);
      jsonRpc = _conf.getStringValue("JSONRPC");
   }

   public AthenaRequest handleRequest(Long id, String method,
                                      JSONArray params) throws Exception {
      return null;
   }

   @Override
   public String buildResponse(AthenaResponse athenaResponse, String txHash, String method) {
      return null;
   }

   @SuppressWarnings("unchecked")
   public String buildLocalResponse(Object data, Long id) {
      JSONObject result = new JSONObject();
      result.put("id", id);
      result.put("jsonrpc", jsonRpc);
      result.put("result", data);
      return result.toJSONString();
   }
}

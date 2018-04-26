package Servlets.EthRPCHandlers;

import org.json.simple.JSONArray;
import org.json.simple.JSONObject;

import com.vmware.athena.Athena;
import com.vmware.athena.Athena.AthenaRequest;
import com.vmware.athena.Athena.AthenaResponse;
import com.vmware.athena.Athena.EthResponse;

import Servlets.APIHelper;
import configurations.ConfigurationFactory;
import configurations.IConfiguration;
import configurations.ConfigurationFactory.ConfigurationType;

public class EthGetStorageAtHandler implements AbstractEthRPCHandler {
   private String jsonRpc;
   private IConfiguration _conf;

   public EthGetStorageAtHandler() {
      _conf = ConfigurationFactory.getConfiguration(ConfigurationType.File);
      jsonRpc = _conf.getStringValue("JSONRPC");
   }

   public AthenaRequest handleRequest(Long id, String method,
                                      JSONArray params) throws Exception {
      Athena.EthRequest.Builder b = Athena.EthRequest.newBuilder();
      b.setId(id);
      b.setAddrTo(APIHelper.hexStringToBinary((String) params.get(0)));

      String p = (String) params.get(1);
      String s = APIHelper.padZeroes(p);
      b.setData(APIHelper.hexStringToBinary(s));

      Athena.EthRequest athenaEthRequest = b.build();
      // Envelope the request object into an athena request object.
      final Athena.AthenaRequest athenarequestObj
         = Athena.AthenaRequest.newBuilder()
                               .addEthRequest(athenaEthRequest)
                               .build();
      return athenarequestObj;
   }

   @SuppressWarnings("unchecked")
   @Override
   public String buildResponse(AthenaResponse athenaResponse, String txHash, String method) {
      EthResponse ethResponse = athenaResponse.getEthResponse(0);

      JSONObject respObject = new JSONObject();
      respObject.put("id", ethResponse.getId());
      respObject.put("jsonrpc", jsonRpc);
      respObject.put("result",
                     APIHelper.binaryStringToHex(ethResponse.getData()));
      return respObject.toJSONString();
   }

   @Override
   public String buildLocalResponse(Object data, Long id) {
      return null;
   }
}

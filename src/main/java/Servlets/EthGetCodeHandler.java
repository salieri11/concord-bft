package Servlets;

import org.json.simple.JSONArray;
import org.json.simple.JSONObject;

import com.vmware.athena.Athena;
import com.vmware.athena.Athena.AthenaRequest;
import com.vmware.athena.Athena.AthenaResponse;
import com.vmware.athena.Athena.EthResponse;
import com.vmware.athena.Athena.EthRequest.EthMethod;

import configurations.ConfigurationFactory;
import configurations.IConfiguration;
import configurations.ConfigurationFactory.ConfigurationType;

public class EthGetCodeHandler implements IEthRPC {
   private String jsonRpc;
   private IConfiguration _conf;

   public EthGetCodeHandler() {
      _conf = ConfigurationFactory.getConfiguration(ConfigurationType.File);
      jsonRpc = _conf.getStringValue("JSONRPC");
   }

   public AthenaRequest handleRequest(Long id, String method,
                                      JSONArray params) throws Exception {
      Athena.EthRequest.Builder b = Athena.EthRequest.newBuilder();
      b.setId(id);
      b.setMethod(EthMethod.GET_CODE);
      b.setAddrTo(APIHelper.hexStringToBinary((String) params.get(0)));
      // ignoring "block" argument for now

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
      JSONObject respObject = new JSONObject();
      EthResponse ethResponse = athenaResponse.getEthResponse(0);
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

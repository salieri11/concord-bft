package Servlets.EthRPCHandlers;

import org.json.simple.JSONArray;
import org.json.simple.JSONObject;

import com.vmware.athena.Athena;
import com.vmware.athena.Athena.AthenaRequest;
import com.vmware.athena.Athena.AthenaResponse;
import com.vmware.athena.Athena.EthResponse;
import com.vmware.athena.Athena.EthRequest.EthMethod;

import Servlets.APIHelper;
import configurations.ConfigurationFactory;
import configurations.IConfiguration;
import configurations.ConfigurationFactory.ConfigurationType;

public class EthGetTxReceiptHandler implements AbstractEthRPCHandler {
   private String jsonRpc;
   private IConfiguration _conf;

   public EthGetTxReceiptHandler() {
      _conf = ConfigurationFactory.getConfiguration(ConfigurationType.File);
      jsonRpc = _conf.getStringValue("JSONRPC");
   }

   public AthenaRequest handleRequest(Long id, String method,
                                      JSONArray params) throws Exception {
      Athena.EthRequest.Builder b = Athena.EthRequest.newBuilder();
      b.setId(id);
      b.setMethod(EthMethod.GET_TX_RECEIPT);
      String txHash = (String) params.get(0);
      b.setData(APIHelper.hexStringToBinary(txHash));
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

      JSONObject result = new JSONObject();
      result.put("status",
                 "0x" + Integer.toString(ethResponse.getStatus(), 16));
      result.put("transactionHash", txHash);
      if (ethResponse.hasContractAddress()) {
         result.put("contractAddress",
                    APIHelper.binaryStringToHex(ethResponse.getContractAddress()));
      } else {
         result.put("contractAddress", null);
      }
      respObject.put("result", result);
      return respObject.toJSONString();
   }

   @Override
   public String buildLocalResponse(Object data, Long id) {
      return null;
   }
}

package Servlets;

import org.apache.log4j.Logger;
import org.json.simple.JSONArray;
import org.json.simple.JSONObject;

import com.google.protobuf.ByteString;
import com.vmware.athena.Athena;
import com.vmware.athena.Athena.AthenaRequest;
import com.vmware.athena.Athena.AthenaResponse;
import com.vmware.athena.Athena.EthResponse;
import com.vmware.athena.Athena.EthRequest.EthMethod;

import configurations.ConfigurationFactory;
import configurations.IConfiguration;
import configurations.ConfigurationFactory.ConfigurationType;

public class EthSendTxHandler implements IEthRPC {
   private static Logger logger = Logger.getLogger(EthSendTxHandler.class);
   private String jsonRpc;
   private IConfiguration _conf;

   public EthSendTxHandler() {
      _conf = ConfigurationFactory.getConfiguration(ConfigurationType.File);
      jsonRpc = _conf.getStringValue("JSONRPC");
   }

   public AthenaRequest handleRequest(Long id, String method,
                                      JSONArray params) throws Exception {

      boolean isSendTx = false;
      Athena.EthRequest.Builder b = Athena.EthRequest.newBuilder();
      b.setId(id);

      if (method.equals(_conf.getStringValue("SendTransaction_Name"))) {
         b.setMethod(EthMethod.SEND_TX);
         isSendTx = true;
      } else {
         b.setMethod(EthMethod.CALL_CONTRACT);
      }

      String from = null, to = null, data = null, value = null;
      JSONObject obj = (JSONObject) params.get(0);

      if (obj.containsKey("from")) {
         from = (String) obj.get("from");
      }
      if (isSendTx && from == null) {
         logger.error("From field missing in params");
         throw new Exception("'from' must be specified");
      }

      if (from != null) {
         ByteString fromAddr = APIHelper.hexStringToBinary(from);
         b.setAddrFrom(fromAddr);
      }

      if (obj.containsKey("to")) {
         to = (String) obj.get("to");
      }
      if (!isSendTx && to == null) {
         logger.error("To field missing in params");
         throw new Exception("'to' must be specified");
      }

      if (to != null) {
         ByteString toAddr = APIHelper.hexStringToBinary(to);
         b.setAddrTo(toAddr);
      }

      if (obj.containsKey("data")) {
         data = (String) obj.get("data");
      }

      if (data != null) {
         ByteString dataBytes = APIHelper.hexStringToBinary(data);
         b.setData(dataBytes);
      }

      if (obj.containsKey("value")) {
         value = (String) obj.get("value");
      }

      if (value != null) {
         ByteString valueBytes
            = APIHelper.hexStringToBinary(APIHelper.padZeroes(value));
         b.setValue(valueBytes);
      }

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
      // Set method specific responses
      respObject.put("result",
                     APIHelper.binaryStringToHex(ethResponse.getData()));
      return respObject.toJSONString();
   }

   @Override
   public String buildLocalResponse(Object data, Long id) {
      return null;
   }
}

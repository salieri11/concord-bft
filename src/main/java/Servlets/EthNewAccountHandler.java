package Servlets;

import java.io.UnsupportedEncodingException;
import java.nio.charset.StandardCharsets;

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

public class EthNewAccountHandler implements IEthRPC {
   private static Logger logger = Logger.getLogger(EthNewAccountHandler.class);
   private String jsonRpc;
   private IConfiguration _conf;

   public EthNewAccountHandler() {
      _conf = ConfigurationFactory.getConfiguration(ConfigurationType.File);
      jsonRpc = _conf.getStringValue("JSONRPC");
   }

   public AthenaRequest handleRequest(Long id, String method,
                                      JSONArray params) throws Exception {

      Athena.EthRequest.Builder b = Athena.EthRequest.newBuilder();
      b.setId(id);

      b.setMethod(EthMethod.NEW_ACCOUNT);
      String passphrase = (String) params.get(0);

      try {
         b.setData(ByteString.copyFrom(passphrase,
                                       StandardCharsets.UTF_8.name()));
      } catch (UnsupportedEncodingException e) {
         logger.error("Invalid passphrase");
         throw e;
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
      respObject.put("result",
                     APIHelper.binaryStringToHex(ethResponse.getData()));
      return respObject.toJSONString();
   }

   @Override
   public String buildLocalResponse(Object data, Long id) {
      return null;
   }
}

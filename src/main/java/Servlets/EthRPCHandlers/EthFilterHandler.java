package Servlets.EthRPCHandlers;

import org.apache.log4j.Logger;
import org.json.simple.JSONArray;
import org.json.simple.JSONObject;

import com.google.protobuf.ByteString;
import com.vmware.athena.Athena;
import com.vmware.athena.Athena.AthenaRequest;
import com.vmware.athena.Athena.AthenaResponse;
import com.vmware.athena.Athena.EthResponse;
import com.vmware.athena.Athena.FilterRequest;
import com.vmware.athena.Athena.FilterRequest.FilterRequestType;

import Servlets.APIHelper;

import com.vmware.athena.Athena.FilterResponse;
import com.vmware.athena.Athena.EthRequest.EthMethod;

import configurations.ConfigurationFactory;
import configurations.IConfiguration;
import configurations.ConfigurationFactory.ConfigurationType;

public class EthFilterHandler implements AbstractEthRPCHandler {
   private static Logger logger = Logger.getLogger(EthFilterHandler.class);
   private String jsonRpc;
   private IConfiguration _conf;

   public EthFilterHandler() {
      _conf = ConfigurationFactory.getConfiguration(ConfigurationType.File);
      jsonRpc = _conf.getStringValue("JSONRPC");
   }

   public AthenaRequest handleRequest(Long id, String method,
                                      JSONArray params) throws Exception {

      Athena.EthRequest.Builder b = Athena.EthRequest.newBuilder();
      b.setId(id);
      b.setMethod(EthMethod.FILTER_REQUEST);

      if (method.equals(_conf.getStringValue("NewFilter_Name"))) {
         // TODO: handle new filter
         logger.warn("eth_newFilter method is not implemented yet");
      } else if (method.equals(_conf.getStringValue("NewBlockFilter_Name"))) {
         FilterRequest.Builder fbuilder = FilterRequest.newBuilder();
         fbuilder.setType(FilterRequestType.NEW_BLOCK_FILTER);
         b.setFilterRequest(fbuilder.build());
      } else if (method.equals(_conf.getStringValue("NewPendingTransactionFilter_Name"))) {
         // TODO: handle new pending transaction filter
         logger.warn("eth_newPendingTransactionFilter method is not"
            + "implemented yet");
      } else if (method.equals(_conf.getStringValue("FilterChange_Name"))) {
         FilterRequest.Builder fbuilder = FilterRequest.newBuilder();
         fbuilder.setType(FilterRequestType.FILTER_CHANGE_REQUEST);
         fbuilder.setFilterId(APIHelper.hexStringToBinary((String) params.get(0)));
         b.setFilterRequest(fbuilder.build());
      } else if (method.equals(_conf.getStringValue("UninstallFilter_Name"))) {
         FilterRequest.Builder fbuilder = FilterRequest.newBuilder();
         fbuilder.setType(FilterRequestType.UNINSTALL_FILTER);
         fbuilder.setFilterId(APIHelper.hexStringToBinary((String) params.get(0)));
         b.setFilterRequest(fbuilder.build());
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
   public String buildResponse(AthenaResponse athenaResponse, String txHash,
                               String method) {
      EthResponse ethResponse = athenaResponse.getEthResponse(0);
      JSONObject respObject = new JSONObject();
      respObject.put("id", ethResponse.getId());
      respObject.put("jsonrpc", jsonRpc);
      Object result = null;

      if (method.equals(_conf.getStringValue("NewFilter_Name"))
         || method.equals(_conf.getStringValue("NewBlockFilter_Name"))
         || method.equals(_conf.getStringValue("NewPendingTransactionFilter_Name"))) {
         result = APIHelper.binaryStringToHex(ethResponse.getFilterResponse()
                                                         .getFilterId());
      } else if (method.equals(_conf.getStringValue("FilterChange_Name"))) {
         JSONArray arr = new JSONArray();
         FilterResponse fresponse = ethResponse.getFilterResponse();
         for (ByteString hash : fresponse.getBlockHashesList()) {
            arr.add(APIHelper.binaryStringToHex(hash));
         }
         result = arr;
      } else {
         result = ethResponse.getFilterResponse().getSuccess();
      }
      respObject.put("result", result);
      return respObject.toJSONString();
   }

   @Override
   public String buildLocalResponse(Object data, Long id) {
      return null;
   }
}

package Servlets.EthRPCHandlers;

import org.apache.log4j.Logger;
import org.json.simple.JSONArray;
import org.json.simple.JSONObject;

import com.google.protobuf.ByteString;
import com.vmware.athena.Athena;
import com.vmware.athena.Athena.EthRequest.EthMethod;
import com.vmware.athena.Athena.EthResponse;
import com.vmware.athena.Athena.FilterRequest;
import com.vmware.athena.Athena.FilterRequest.FilterRequestType;
import com.vmware.athena.Athena.FilterResponse;

import Servlets.APIHelper;
import Servlets.EthDispatcher;

public class EthFilterHandler extends AbstractEthRPCHandler {

   private static Logger logger = Logger.getLogger(EthFilterHandler.class);

   public void buildRequest(Athena.AthenaRequest.Builder builder,
                            JSONObject requestJson) throws Exception {
      Athena.EthRequest.Builder b = initializeRequestObject(requestJson);
      String ethMethodName = EthDispatcher.getEthMethodName(requestJson);
      JSONArray params = extractRequestParams(requestJson);

      b.setMethod(EthMethod.FILTER_REQUEST);

      if (ethMethodName.equals(_conf.getStringValue("NewFilter_Name"))) {
         // TODO: handle new filter
         logger.warn("eth_newFilter method is not implemented yet");
      } else if (ethMethodName.equals(_conf.getStringValue("NewBlockFilter_Name"))) {
         FilterRequest.Builder fb = FilterRequest.newBuilder();
         fb.setType(FilterRequestType.NEW_BLOCK_FILTER);
         b.setFilterRequest(fb.build());
      } else if (ethMethodName.equals(_conf.getStringValue("NewPendingTransactionFilter_Name"))) {
         // TODO: handle new pending transaction filter
         logger.warn("eth_newPendingTransactionFilter method is not"
            + "implemented yet");
      } else if (ethMethodName.equals(_conf.getStringValue("FilterChange_Name"))) {
         FilterRequest.Builder fb = FilterRequest.newBuilder();
         fb.setType(FilterRequestType.FILTER_CHANGE_REQUEST);
         fb.setFilterId(APIHelper.hexStringToBinary((String) params.get(0)));
         b.setFilterRequest(fb.build());
      } else if (ethMethodName.equals(_conf.getStringValue("UninstallFilter_Name"))) {
         FilterRequest.Builder fb = FilterRequest.newBuilder();
         fb.setType(FilterRequestType.UNINSTALL_FILTER);
         fb.setFilterId(APIHelper.hexStringToBinary((String) params.get(0)));
         b.setFilterRequest(fb.build());
      }

      Athena.EthRequest athenaEthRequest = b.build();

      builder.addEthRequest(athenaEthRequest);
   }

   @SuppressWarnings("unchecked")
   public JSONObject buildResponse(Athena.AthenaResponse athenaResponse,
                                   JSONObject requestJson) throws Exception {
      try {
         EthResponse ethResponse = athenaResponse.getEthResponse(0);
         JSONObject respObject = initializeResponseObject(ethResponse);
         String ethMethodName = EthDispatcher.getEthMethodName(requestJson);

         if (ethMethodName.equals(_conf.getStringValue("NewFilter_Name"))
            || ethMethodName.equals(_conf.getStringValue("NewBlockFilter_Name"))
            || ethMethodName.equals(_conf.getStringValue("NewPendingTransactionFilter_Name"))) {
            respObject.put("result",
                           APIHelper.binaryStringToHex(ethResponse.getFilterResponse()
                                                                  .getFilterId()));
         } else if (ethMethodName.equals(_conf.getStringValue("FilterChange_Name"))) {
            JSONArray arr = new JSONArray();
            FilterResponse fresponse = ethResponse.getFilterResponse();
            for (ByteString hash : fresponse.getBlockHashesList()) {
               arr.add(APIHelper.binaryStringToHex(hash));
            }
            respObject.put("result", arr);
         } else {
            respObject.put("result",
                           ethResponse.getFilterResponse().getSuccess());
         }
         return respObject;
      } catch (Exception e) {
         logger.error("Exception in Filter Handler build response", e);
         throw e;
      }
   }

}

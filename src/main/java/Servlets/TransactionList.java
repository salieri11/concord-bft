package Servlets;

import java.io.IOException;
import java.util.Map;

import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import org.apache.log4j.Logger;
import org.json.simple.JSONArray;
import org.json.simple.JSONAware;
import org.json.simple.JSONObject;

import com.google.protobuf.ByteString;
import com.vmware.athena.Athena;

public class TransactionList extends BaseServlet {

   private static final long serialVersionUID = 1L;
   private final static Logger logger = Logger.getLogger(TransactionList.class);
   private final String transactionListEndpoint
      = _conf.getStringValue("TransactionList_Endpoint");

   /**
    * Services a get request. Constructs a protobuf request of type
    * Transactionlist request (enveloped in an athena request) as defined in
    * athena.proto. Sends this request to Athena. Parses the response and
    * converts it into json for responding to the client.
    *
    * @param request
    *           The request received by the servlet
    * @param response
    *           The response object used to respond to the client
    * @throws IOException
    */
   @Override
   protected void doGet(final HttpServletRequest request,
                        final HttpServletResponse response) throws IOException {
      Map<String, String[]> paramMap = request.getParameterMap();
      ByteString latest = null;
      long count = _conf.getLongValue("TransactionList_DefaultCount");
      Athena.AthenaRequest athenaRequest;
      
      try {
         Athena.TransactionListRequest.Builder txListReqBuilder
                 = Athena.TransactionListRequest.newBuilder();
   
         if (paramMap.containsKey("latest")) {
            latest = APIHelper.hexStringToBinary(paramMap.get("latest")[0]);
            txListReqBuilder.setLatest(latest);
         }
   
         if (paramMap.containsKey("count")) {
            if (paramMap.get("count")[0].chars().allMatch(Character::isDigit)) {
               count = Long.parseLong(paramMap.get("count")[0]);
            }
         }
         txListReqBuilder.setCount(count);
         logger.info("requested count: " + count);
   
         athenaRequest
                 = Athena.AthenaRequest.newBuilder()
                 .setTransactionListRequest(txListReqBuilder.build())
                 .build();
   
         processGet(athenaRequest, response, logger);
      } catch (Exception e) {
         logger.warn("Exception in transaction list", e);
         processResponse(response,
                 APIHelper.errorJSON(e.getMessage()).toJSONString(),
                 HttpServletResponse.SC_BAD_REQUEST,
                 logger);
      }
   }

   /**
    * Parses the Protocol Buffer response from Athena and converts it into JSON.
    *
    * @param athenaResponse
    *           Protocol Buffer object containing Athena's reponse
    * @return Response in JSON format
    */
   @SuppressWarnings("unchecked")
   @Override
   protected JSONAware parseToJSON(Athena.AthenaResponse athenaResponse) {
      // Extract the transaction response from
      // the athena reponse envelope.
      Athena.TransactionListResponse txListResponse
         = athenaResponse.getTransactionListResponse();

      // Construct the reponse JSON object.
      JSONObject responseJSON = new JSONObject();
      JSONArray trArray = new JSONArray();
      for (ByteString hash : txListResponse.getTransactionList()) {
         JSONObject tr = new JSONObject();
         tr.put("hash", APIHelper.binaryStringToHex(hash));
         tr.put("url",
                transactionListEndpoint + "/"
                   + APIHelper.binaryStringToHex(hash));
         trArray.add(tr);
      }
      responseJSON.put("transactions", trArray);
      if (txListResponse.hasNext()) {
         responseJSON.put("next",
                          transactionListEndpoint + "?latest="
                             + APIHelper.binaryStringToHex(txListResponse.getNext()));
      }
      return responseJSON;
   }
}

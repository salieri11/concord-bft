/**
 * url endpoint : /api/athena/blocks/{N}
 *
 * Used to fetch a specific block from the chain.
 *
 * This servlet is used to send BlockNumber Requests to Athena and to parse the
 * responses into JSON. A TCP socket connection is made to Athena and requests
 * and responses are encoded in the Google Protocol Buffer format.
 *
 */
package Servlets;

import com.vmware.athena.*;
import com.vmware.athena.Athena.TransactionResponse;

import io.undertow.util.StatusCodes;
import java.io.IOException;
import java.util.List;

import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import org.apache.log4j.Logger;
import org.json.simple.JSONArray;
import org.json.simple.JSONObject;

/**
 * Servlet class.
 */
public final class BlockNumber extends BaseServlet {
   private static final long serialVersionUID = 1L;
   private final static Logger _logger = Logger.getLogger(BlockNumber.class);

   /**
    * Services a get request. Constructs a protobuf request of type blocknumber
    * request (enveloped in an athena request) as defined in athena.proto. Sends
    * this request to Athena. Parses the response and converts it into json for
    * sendiong to clientß
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
      // Read the requested block number from the uri
      Long number = null;
      try {
         String uri = request.getRequestURI();

         // Allow trailing /
         if (uri.charAt(uri.length() - 1) == '/') {
            uri = uri.substring(0, uri.length() - 1);
         }

         String urlParam = uri.substring(uri.lastIndexOf('/') + 1);
         number = Long.parseLong(urlParam);
      } catch (NumberFormatException e) {
         _logger.error("Invalid block number");
         processResponse(response, "error", StatusCodes.BAD_REQUEST, _logger);
      }

      // Construct a blockNumberRequest object. Set its start field.
      final Athena.BlockRequest blockRequestObj
         = Athena.BlockRequest.newBuilder().setNumber(number).build();

      // Envelope the blockRequest object into an athena object.
      final Athena.AthenaRequest athenaRequestObj
         = Athena.AthenaRequest.newBuilder()
                               .setBlockRequest(blockRequestObj)
                               .build();

      processGet(athenaRequestObj, response, _logger);
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
   protected JSONObject parseToJSON(Athena.AthenaResponse athenaResponse) {

      // Extract the blocknumber response
      // from the athena reponse envelope.
      Athena.BlockResponse blockResponse = athenaResponse.getBlockResponse();

      JSONArray transactionArr = new JSONArray();

      List<TransactionResponse> list
         = (List<TransactionResponse>) blockResponse.getTransactionList();

      for (TransactionResponse t : list) {
         String hash = APIHelper.binaryStringToHex(t.getHash());
         JSONObject txJSON = new JSONObject();
         txJSON.put("hash", hash);
         txJSON.put("url",
                    _conf.getStringValue("Transaction_URLPrefix") + hash);
         transactionArr.add(txJSON);
      }

      JSONObject blockObj = new JSONObject();
      blockObj.put("transactions", transactionArr);

      blockObj.put("number", blockResponse.getNumber());

      String hash = APIHelper.binaryStringToHex(blockResponse.getHash());
      String parentHash
         = APIHelper.binaryStringToHex(blockResponse.getParentHash());

      blockObj.put("hash", hash);
      blockObj.put("parentHash", parentHash);
      blockObj.put("nonce",
                   APIHelper.binaryStringToHex(blockResponse.getNonce()));
      blockObj.put("size", blockResponse.getSize());

      return blockObj;
   }
}

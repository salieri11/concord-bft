/**
 * url endpoint : /api/athena/blocks/{N}
 * Used to fetch a specific block from the chain.
 *
 * This servlet is used to send BlockNumber Requests to Athena and to parse
 * the responses into JSON. A TCP socket connection is made to Athena
 * and requests and responses are encoded in the Google Protocol Buffer
 * format.
 *
 * Athena, by default, runs on port 5458.
 * TODO : Handle the case of no/incorrect response from Athena
 */
package Servlets;

import com.google.protobuf.ProtocolStringList;
import com.vmware.athena.*;
import io.undertow.util.StatusCodes;
import java.io.IOException;
import java.util.Iterator;
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
   private final static Logger _logger =
      Logger.getLogger(BlockNumber.class);

   /**
    * Services a get request. Constructs a protobuf request
    * of type blocknumber request (enveloped in an athena request)
    * as defined in athena.proto. Sends this request to Athena.
    * Parses the response and converts it into json for
    * sendiong to clientß
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
      Long index = null;
      try {
         String uri = request.getRequestURI();
         String urlParam = uri.substring(uri.lastIndexOf('/') + 1);
         index = Long.parseLong(urlParam);
      } catch (NumberFormatException e) {
         _logger.error("Invalid block number");
         processResponse(response, "error",StatusCodes.BAD_REQUEST,
               _logger);
      }

      // Construct a blockNumberRequest object. Set its start field.
      final Athena.BlockNumberRequest blockNumberRequestObj =
            Athena.BlockNumberRequest.newBuilder()
            .setIndex(index)
            .build();

      // Envelope the blockNumberRequest object into an athena object.
      final Athena.AthenaRequest athenarequestObj =
            Athena.AthenaRequest.newBuilder()
            .setBlockNumberRequest(blockNumberRequestObj)
            .build();

      // this block will be replaced by the line below
         Athena.AthenaResponse athenaResponse = null;
         JSONObject blockNumberResponse = null;
      
         // receive response from Athena
         athenaResponse = receiveFromAthenaMock(); // This is temporary.
         // athenaResponse = receiveFromAthena(inFromAthena); //Coming soon
         blockNumberResponse = parseToJSON(athenaResponse);
         // Set client response header
         response.setHeader("Content-Transfer-Encoding", "UTF-8");
         response.setContentType("application/json");
         // Respond to client.
         response.getWriter().write(blockNumberResponse.toString());
      // block ends////////////////////////////////////////
      
      // this line will replace the block marked above
      // processGet(athenarequestObj, response, _logger);
   }

   /**
    * Note : This is a temporary function which mocks Athena's response.
    *
    * @return
    */
   public Athena.AthenaResponse receiveFromAthenaMock() {
      final Athena.BlockDetailed blockDetailedObj = Athena.BlockDetailed
            .newBuilder()
            .setNumber(1)
            .setHash("hash")
            .setParentHash("parentHash")
            .setNonce("Nonce")
            .setSize(50)
            .addTransactions("transaction1")
            .addTransactions("transaction2")
            .build();

      // Construct a blockNumberResponse object.
      final Athena.BlockNumberResponse blockNumberResponseObj =
            Athena.BlockNumberResponse.newBuilder()
            .setBlock(blockDetailedObj)
            .build();

      // Envelope the blockNumberResponse object into an athena object.
      final Athena.AthenaResponse athenaresponseObj =
            Athena.AthenaResponse.newBuilder()
            .setBlockNumberResponse(blockNumberResponseObj)
            .build();

      return athenaresponseObj;
   }

   /**
    * Parses the Protocol Buffer response from Athena
    * and converts it into JSON.
    * @param athenaResponse
    *        Protocol Buffer object containing Athena's reponse
    * @return Response in JSON format
   */
   @SuppressWarnings("unchecked")
   @Override
   protected JSONObject parseToJSON(
         Athena.AthenaResponse athenaResponse) {

      // Extract the blocknumber response
      // from the athena reponse envelope.
      Athena.BlockNumberResponse blockNumberResponse = athenaResponse
            .getBlockNumberResponse();

      // Read the block from the blocknumber response object.
      Athena.BlockDetailed block = blockNumberResponse.getBlock();

      JSONArray transactionArr = new JSONArray();
      ProtocolStringList transactionList = block.getTransactionsList();
      Iterator<String> it = transactionList.iterator();

      while (it.hasNext()) {
         transactionArr.add(it.next());
      }

      JSONObject blockObj = new JSONObject();
      blockObj.put("transactions", transactionArr);

      blockObj.put("number", block.getNumber());
      blockObj.put("hash", block.getHash());
      blockObj.put("parentHash", block.getParentHash());
      blockObj.put("nonce", block.getNonce());
      blockObj.put("size", block.getSize());

      // Construct the reponse JSON object.
      JSONObject responseJson = new JSONObject();
      responseJson.put("block", blockObj);

      return responseJson;
   }
}

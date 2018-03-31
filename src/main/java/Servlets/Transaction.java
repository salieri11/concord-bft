/**
 * url endpoint : /api/athena/transaction/{hash}
 * Used to get a specific transaction by its hash.
 *
 * This servlet is used to send Transaction Requests to Athena and to parse
 * the responses into JSON. A TCP socket connection is made to Athena
 * and requests and responses are encoded in the Google Protocol Buffer
 * format.
 *
 * Athena, by default, runs on port 5458.
 * TODO : Handle the case of no/incorrect response from Athena
 */
package Servlets;

import com.google.protobuf.ByteString;
import com.vmware.athena.*;

import connections.AthenaTCPConnection;
import io.undertow.util.StatusCodes;

import java.io.IOException;
import java.io.PrintWriter;

import javax.servlet.http.HttpServlet;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import org.apache.log4j.Logger;
import org.json.simple.JSONObject;
import org.json.simple.parser.ParseException;

/**
 * Servlet class.
 */
public final class Transaction extends BaseServlet {
   private static final long serialVersionUID = 1L;
   private static final Logger logger = 
         Logger.getLogger(Transaction.class);

   /**
    * Services a get request. Constructs a protobuf request of type
    *  transaction request (enveloped in an athena request)
    *  as defined in athena.proto.
    *  Sends this request to Athena. Parses the response 
    *  and converts it into json for responding to the client.
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

      // Read the requested transaction hash from the uri
      String uri = request.getRequestURI();
      String hash = uri.substring(uri.lastIndexOf('/') + 1);

      ByteString hashBytes;
      try {
         hashBytes = APIHelper.hexStringToBinary(hash);
      } catch (Exception e) {
         logger.error("Invalid Hash");
         processResponse(response,
               "error", 
               StatusCodes.BAD_REQUEST,
               logger);
         return;
      }

      if (hashBytes == null) {
         logger.error("Invalid hash in request");
         processResponse(response,
               "error",
               StatusCodes.BAD_REQUEST,
               logger);
         return;
      }

      // Construct a transaction request object.
      final Athena.TransactionRequest txRequestObj =
            Athena.TransactionRequest
               .newBuilder().setHashParam(hashBytes).build();

      // Envelope the transaction request object into an athena object.
      final Athena.AthenaRequest athenarequestObj = Athena.AthenaRequest
               .newBuilder().setTransactionRequest(txRequestObj).build();

      processGet(athenarequestObj, response, logger);
   }

   /**
    * Parses the Protocol Buffer response from Athena and converts it
    *  into JSON.
    *
    * @param athenaResponse
    *           Protocol Buffer object containing Athena's reponse
    * @return Response in JSON format
    */
   @SuppressWarnings("unchecked")
   @Override
   protected JSONObject parseToJSON(Athena.AthenaResponse athenaResponse)
   {

      // Extract the transaction response from
      // the athena reponse envelope.
      Athena.TransactionResponse txResponse = athenaResponse
               .getTransactionResponse();

      // Construct the reponse JSON object.
      JSONObject responseJson = new JSONObject();

      responseJson.put("hash", txResponse.getHash());
      responseJson.put("from", txResponse.getFrom());
      responseJson.put("to", txResponse.getTo());
      responseJson.put("value", txResponse.getValue());
      responseJson.put("input", txResponse.getInput());
      responseJson.put("blockHash", txResponse.getBlockHash());
      responseJson.put("blockNumber", txResponse.getBlockNumber());
      responseJson.put("transactionHash",
            txResponse.getTransactionIndex());
      responseJson.put("blockNumber", txResponse.getBlockNumber());
      responseJson.put("transactionIndex",
            txResponse.getTransactionIndex());
      responseJson.put("nonce", txResponse.getNonce());

      return responseJson;
   }
}

/**
 * url endpoint : /api/athena/transaction/{hash}
 *
 * Used to get a specific transaction by its hash.
 *
 * This servlet is used to send Transaction Requests to Athena and to parse the
 * responses into JSON. A TCP socket connection is made to Athena and requests
 * and responses are encoded in the Google Protocol Buffer format.
 *
 * TODO : Handle the case of no/incorrect response from Athena
 */
package Servlets;

import java.io.IOException;

import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import org.apache.log4j.Logger;
import org.json.simple.JSONObject;

import com.google.protobuf.ByteString;
import com.vmware.athena.Athena;

import io.undertow.util.StatusCodes;

/**
 * Servlet class.
 */
public final class Transaction extends BaseServlet {
   private static final long serialVersionUID = 1L;
   private static final Logger logger = Logger.getLogger(Transaction.class);

   protected static JSONObject
             buildTransactionResponseJSON(Athena.TransactionResponse tr) {
      // Construct the reponse JSON object.
      JSONObject responseJSON = new JSONObject();

      responseJSON.put("hash", APIHelper.binaryStringToHex(tr.getHash()));
      responseJSON.put("from", APIHelper.binaryStringToHex(tr.getFrom()));
      responseJSON.put("block_hash",
                       APIHelper.binaryStringToHex(tr.getBlockHash()));
      responseJSON.put("block_number", tr.getBlockNumber());

      if (tr.hasTo()) {
         responseJSON.put("to", APIHelper.binaryStringToHex(tr.getTo()));
      }

      if (tr.hasContractAddress()) {
         responseJSON.put("contract_address",
                          APIHelper.binaryStringToHex(tr.getContractAddress()));
      }

      if (tr.hasValue()) {
         responseJSON.put("value", Long.toString(tr.getValue()));
      }

      if (tr.hasInput()) {
         responseJSON.put("input", APIHelper.binaryStringToHex(tr.getInput()));
      }
      responseJSON.put("nonce", tr.getNonce());

      return responseJSON;
   }

   /**
    * Services a get request. Constructs a protobuf request of type transaction
    * request (enveloped in an athena request) as defined in athena.proto. Sends
    * this request to Athena. Parses the response and converts it into json for
    * responding to the client.
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

      // Allow trailing /
      if (uri.charAt(uri.length() - 1) == '/') {
         uri = uri.substring(0, uri.length() - 1);
      }

      String hash = uri.substring(uri.lastIndexOf('/') + 1);

      ByteString hashBytes;
      try {
         hashBytes = APIHelper.hexStringToBinary(hash);
      } catch (Exception e) {
         logger.error("Invalid Hash");
         processResponse(response, "error", StatusCodes.BAD_REQUEST, logger);
         return;
      }

      if (hashBytes == null) {
         logger.error("Invalid hash in request");
         processResponse(response, "error", StatusCodes.BAD_REQUEST, logger);
         return;
      }

      // Construct a transaction request object.
      final Athena.TransactionRequest txRequestObj
         = Athena.TransactionRequest.newBuilder().setHash(hashBytes).build();

      // Envelope the transaction request object into an athena object.
      final Athena.AthenaRequest athenarequestObj
         = Athena.AthenaRequest.newBuilder()
                               .setTransactionRequest(txRequestObj)
                               .build();

      processGet(athenarequestObj, response, logger);
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

      // Extract the transaction response from
      // the athena reponse envelope.
      Athena.TransactionResponse txResponse
         = athenaResponse.getTransactionResponse();

      JSONObject responseJSON = buildTransactionResponseJSON(txResponse);

      // Athena EVM has status code '0' for success and other Positive
      // values to denote error. However, for JSON RPC '1' is success
      // and '0' is failure. Here we need to reverse status value of athena
      // response before returning it.
      responseJSON.put("status", txResponse.getStatus() == 0 ? 1 : 0);

      return responseJSON;
   }
}

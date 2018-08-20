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

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.json.simple.JSONAware;
import org.json.simple.JSONObject;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.stereotype.Controller;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestMethod;

import com.google.protobuf.ByteString;
import com.vmware.athena.Athena;

/**
 * Servlet class.
 */
@Controller
public final class Transaction extends BaseServlet {

   private static final long serialVersionUID = 1L;
   private static final Logger logger = LogManager.getLogger(Transaction.class);

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
   @RequestMapping(path = "/api/athena/transactions/{hash}",
                   method = RequestMethod.GET)
   protected ResponseEntity<JSONAware>
             doGet(@PathVariable(value = "hash", required = true) String hash) {
      ResponseEntity<JSONAware> responseEntity;
      ByteString hashBytes = null;
      try {
         hashBytes = APIHelper.hexStringToBinary(hash);
      } catch (Exception e) {
         logger.error("Invalid Hash");
         return new ResponseEntity<>(new JSONObject(),
                                     standardHeaders,
                                     HttpStatus.BAD_REQUEST);
      }

      if (hashBytes == null) {
         logger.error("Invalid hash in request");
         return new ResponseEntity<>(new JSONObject(),
                                     standardHeaders,
                                     HttpStatus.BAD_REQUEST);
      }

      // Construct a transaction request object.
      final Athena.TransactionRequest txRequestObj
         = Athena.TransactionRequest.newBuilder().setHash(hashBytes).build();

      // Envelope the transaction request object into an athena object.
      final Athena.AthenaRequest athenarequestObj
         = Athena.AthenaRequest.newBuilder()
                               .setTransactionRequest(txRequestObj)
                               .build();

      return sendToAthenaAndBuildHelenResponse(athenarequestObj);
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

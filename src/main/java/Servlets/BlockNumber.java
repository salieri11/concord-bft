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

import java.util.List;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.json.simple.JSONArray;
import org.json.simple.JSONAware;
import org.json.simple.JSONObject;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.stereotype.Controller;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestMethod;

import com.vmware.athena.Athena;

/**
 * Servlet class.
 */
@Controller
public class BlockNumber extends BaseServlet {
   private static final long serialVersionUID = 1L;
   private Logger logger = LogManager.getLogger(BlockNumber.class);

   /**
    * Services a get request. Constructs a protobuf request of type blocknumber
    * request (enveloped in an athena request) as defined in athena.proto. Sends
    * this request to Athena. Parses the response and converts it into json for
    * sendiong to client
    *
    * @param block
    *          The block number or block hash
    */
   @RequestMapping(method = RequestMethod.GET,
                   path = "/api/athena/blocks/{block}")
   public ResponseEntity<JSONAware>
          getBlock(@PathVariable("block") String block) {
      // Block can either be a block number or block hash
      // Read the requested block number from the uri
      Long number;
      try {
         final Athena.BlockRequest blockRequestObj;
         // check if param is a number or hash
         if (block.chars().allMatch(Character::isDigit)) {
            number = Long.parseLong(block);
            blockRequestObj
               = Athena.BlockRequest.newBuilder().setNumber(number).build();
         } else {
            blockRequestObj
               = Athena.BlockRequest.newBuilder()
                                    .setHash(APIHelper.hexStringToBinary(block))
                                    .build();
         }

         // Envelope the blockRequest object into an athena object.
         final Athena.AthenaRequest athenaRequestObj
            = Athena.AthenaRequest.newBuilder()
                                  .setBlockRequest(blockRequestObj)
                                  .build();
         return sendToAthenaAndBuildHelenResponse(athenaRequestObj);

      } catch (Exception e) {
         logger.error("Invalid block number or hash");
         return new ResponseEntity<>(APIHelper.errorJSON("Invalid block number or hash"),
                                     standardHeaders,
                                     HttpStatus.BAD_REQUEST);
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
   protected JSONObject parseToJSON(Athena.AthenaResponse athenaResponse) {

      // Extract the blocknumber response
      // from the athena reponse envelope.
      Athena.BlockResponse blockResponse = athenaResponse.getBlockResponse();

      JSONArray transactionArr = new JSONArray();

      List<Athena.TransactionResponse> list
         = (List<Athena.TransactionResponse>) blockResponse.getTransactionList();

      for (Athena.TransactionResponse t : list) {
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

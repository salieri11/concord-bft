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
 * 
 */
package Servlets;

import com.google.protobuf.InvalidProtocolBufferException;
import com.google.protobuf.ProtocolStringList;
import com.vmware.athena.*;

import connections.AthenaConnectionPool;
import connections.AthenaTCPConnection;
import connections.IAthenaConnection;
import io.undertow.util.StatusCodes;

import java.io.DataInputStream;
import java.io.DataOutputStream;
import java.io.IOException;
import java.io.PrintWriter;
import java.util.Iterator;
import java.util.concurrent.locks.ReentrantLock;

import javax.servlet.http.HttpServlet;
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
   private final Logger logger = Logger.getLogger(BlockNumber.class);

   /**
    * Services a get request. Constructs a protobuf request
    * of type blocknumber request (enveloped in an athena request)
    * as defined in athena.proto. Sends this request to Athena. 
    * Parses the response and converts it into json for
    * responding to the client.
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
      Long blockNumber = null;
      try {
         String uri = request.getRequestURI();
         blockNumber = Long.parseLong(
               uri.substring(
                     uri.lastIndexOf('/') + 1));
      } catch (NumberFormatException e) {
         logger.error("Invalid block number");
         response.sendError(StatusCodes.NOT_FOUND);
         throw new NumberFormatException();
      }

      // Construct a blockNumberRequest object. Set its start field.
      final Athena.BlockNumberRequest blockNumberRequestObj =
            Athena.BlockNumberRequest.newBuilder()
            .setIndex(blockNumber)
            .build();

      // Envelope the blockNumberRequest object into an athena object.
      final Athena.AthenaRequest athenarequestObj = 
            Athena.AthenaRequest.newBuilder()
            .setBlockNumberRequest(blockNumberRequestObj)
            .build();

      processGet(athenarequestObj, response, logger);
   }

   /**
    * Note : This is a temporary function which mocks Athena's response.
    * 
    * @return
   */
   public JSONObject receiveFromAthenaMock() {
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

      // Convert Protocol Buffer to JSON.
      JSONObject responseJson = parseToJSON(athenaresponseObj);
      return responseJson;
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
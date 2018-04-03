/**
 * url endpoint : /api/athena/blocks
 * Used to list blocks in the chain, most recent first.
 * 
 * This servlet is used to send BlockList Requests to Athena and to parse
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
import com.vmware.athena.Athena.BlocksListResponse;

import configurations.SystemConfiguration;
import connections.AthenaTCPConnection;
import io.undertow.util.StatusCodes;

import java.io.IOException;
import java.io.PrintWriter;
import java.util.ArrayList;
import java.util.List;
import java.util.Properties;
import java.util.Random;

import javax.servlet.http.HttpServlet;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import org.apache.log4j.Logger;
import org.json.simple.JSONArray;
import org.json.simple.JSONObject;
import org.json.simple.parser.ParseException;

/**
 * Servlet class.
 */
public final class BlockList extends HttpServlet {
   private static final long serialVersionUID = 1L;
   private static AthenaTCPConnection athenaConnection;
   private static Properties config;
   private final Logger logger;

   /**
    * Retrieves the common TCP connection object.
    * 
    * @throws IOException
    * @throws ParseException
    */
   public BlockList() throws IOException, ParseException {
      logger = Logger.getLogger(BlockList.class);
      try {
         athenaConnection = AthenaTCPConnection.getInstance();
      } catch (IOException e) {
         logger.error("Error in creating TCP connection with Athena");
         throw e;
      }

      // Read configurations
      SystemConfiguration s;
      try {
         s = SystemConfiguration.getInstance();
      } catch (IOException e) {
         logger.error("Error in reading configurations");
         throw e;
      }
      config = s.configurations;
   }

   /**
    * Services a get request. Constructs a protobuf request of type blocklist
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
      PrintWriter writer = null;
      try {
         writer = response.getWriter();
      } catch (IOException e) {
         logger.error("Error in retrieving the writer object of the "
                  + "HttpResponse");
         throw e;
      }

      // Read the request params
      Long latest = null;
      Long count = null;

      try {
         latest = Long.parseLong(request.getParameter("latest"));
      } catch (Exception e) {
         // Do nothing as this parameter is optional
      }

      try {
         count = Long.parseLong(request.getParameter("count"));
      } catch (Exception e) {
         // Do nothing as this parameter is optional
      }

      // Construct a blocksListRequest object.
      Athena.BlocksListRequest.Builder b = Athena.BlocksListRequest
               .newBuilder();

      // If end is null, Athena assumes end is the latest block
      if (latest != null) {
         b.setLatest(latest);
      }

      // If listLength is null, request for default no. of blocks
      if (count == null) {
         count = Long.parseLong(config.getProperty("BlockList_DefaultCount"));
      }
      b.setCount(count);

      final Athena.BlocksListRequest blocksListRequestObj = b.build();

      // Envelope the blocksListRequest object into an athena object.
      final Athena.AthenaRequest athenarequestObj = Athena.AthenaRequest
               .newBuilder().setBlocksListRequest(blocksListRequestObj).build();

      Athena.AthenaResponse athenaResponse = null;
      JSONObject blocksListResponse = null;

      // receive response from Athena

      /////////////////// This is temporary./////////////////
      if (latest == null) {
         latest = -1L;
      }
      athenaResponse = receiveFromAthenaMock(count, latest);
      //////////////////////////////////////////////////////

      // Coming soon
      // athenaResponse = athenaConnection.sendToAthena(athenarequestObj);

      blocksListResponse = parseToJSON(athenaResponse);

      // Set client response header
      response.setHeader("Content-Transfer-Encoding", "UTF-8");
      response.setContentType("application/json");

      // Respond to client.
      writer.write(blocksListResponse.toString());
   }

   /**
    * Note : This is a temporary function which mocks Athena's response.
    * 
    * @return
    * @throws Exception
    */
   public Athena.AthenaResponse receiveFromAthenaMock(long count, long latest) {

      int chainLength = (int) count;
      int latestBlock = (int) latest;

      if (latestBlock == -1L) {
         latestBlock = chainLength + 1;
      }

      if (latestBlock > 100) {
         latestBlock = 100;
      }

      if (chainLength >= latestBlock) {
         chainLength = latestBlock - 1;
      }

      ArrayList<FakeBlock> list = new ArrayList<>(chainLength);

      for (int i = chainLength; i > 0; i--) {
         list.add(new FakeBlock(latestBlock));
         latestBlock--;
      }

      Athena.BlocksListResponse.Builder b = Athena.BlocksListResponse
               .newBuilder();

      for (FakeBlock f : list) {
         final Athena.BlockBrief blockBriefObj = Athena.BlockBrief.newBuilder()
                  .setNumber(f.number).setHash(f.hash).build();
         b.addBlocks(blockBriefObj);
      }

      final Athena.BlocksListResponse r = b.build();
      Athena.AthenaResponse athenaResponseObj = Athena.AthenaResponse
               .newBuilder().setBlocksListResponse(r).build();

      return athenaResponseObj;
   }

   /**
    * Parses the Protocol Buffer response from Athena and converts it into JSON.
    * 
    * @param athenaResponse
    *           Protocol Buffer object containing Athena's reponse
    * @return Response in JSON format
    */
   @SuppressWarnings("unchecked")
   private JSONObject parseToJSON(Athena.AthenaResponse athenaResponse) {

      // Extract the blocklist response from the athena reponse envelope.
      Athena.BlocksListResponse blocksListResponse = athenaResponse
               .getBlocksListResponse();

      long earliestBlock = -1L;

      // Read list of blocks from the blocks list response object.
      List<Athena.BlockBrief> blockList = new ArrayList<>();
      blockList = blocksListResponse.getBlocksList();

      JSONArray blockArr = new JSONArray();

      // Iterate through each block and construct a corresponding JSON object
      for (Athena.BlockBrief block : blockList) {
         JSONObject blockJson = new JSONObject();

         long number = block.getNumber();
         String hexString = APIHelper.binaryStringToHex(block.getHash());

         blockJson.put("number", number);
         blockJson.put("hash", hexString);

         String url = hexString.substring(2); // remove the "0x" at the start

         blockJson.put("url", config.getProperty("BlockList_URLPrefix") + url);

         // Store into a JSON array of all blocks.
         blockArr.add(blockJson);
         earliestBlock = number;
      }

      // Construct the reponse JSON object.
      JSONObject responseJson = new JSONObject();
      responseJson.put("blocks", blockArr);
      responseJson.put("next", config.getProperty("BlockList_NextPrefix")
               + (earliestBlock - 1));

      return responseJson;
   }

   /**
    * Used to structure mock responses Temporary
    *
    */
   private class FakeBlock {
      int number;
      ByteString hash;
      char[] hex = { '0', '1', '2', '3', '4', '5', '6', '7', '8', '9', 'A', 'B',
               'C', 'D', 'E', 'F' };

      FakeBlock(int number) {
         this.number = number;

         Random r = new Random(System.currentTimeMillis());
         StringBuilder sb = new StringBuilder();

         for (int i = 0; i < 64; i++) {
            sb.append(hex[r.nextInt(hex.length)]);
         }

         try {
            hash = APIHelper.hexStringToBinary(sb.toString());
         } catch (Exception e) {
            logger.error(
                     "Error in converting from hex string to binary string");
            byte[] temp = new byte[32];
            hash = ByteString.copyFrom(temp);
         }
      }
   }
}
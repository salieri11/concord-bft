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

import com.vmware.athena.*;

import configurations.SystemConfiguration;
import connections.AthenaTCPConnection;

import java.io.IOException;
import java.io.PrintWriter;
import java.util.ArrayList;
import java.util.List;
import java.util.Properties;

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
   private static long start;
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
      start = Long.parseLong(config.getProperty("BlockList_Start"));
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
      // Construct a blocksListRequest object. Set its start field.
      final Athena.BlocksListRequest blocksListRequestObj = Athena.BlocksListRequest
               .newBuilder().setStart(start).build();

      // Envelope the blocksListRequest object into an athena object.
      final Athena.AthenaRequest athenarequestObj = Athena.AthenaRequest
               .newBuilder().setBlocksListRequest(blocksListRequestObj).build();

      Athena.AthenaResponse athenaResponse = null;
      JSONObject blocksListResponse = null;

      // receive response from Athena
      athenaResponse = receiveFromAthenaMock(); // This is temporary.

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
    */
   public Athena.AthenaResponse receiveFromAthenaMock() {
      final Athena.BlockBrief blockBriefObj1 = Athena.BlockBrief.newBuilder()
               .setNumber(1).setHash("hash1").setUrl("url1").build();
      final Athena.BlockBrief blockBriefObj2 = Athena.BlockBrief.newBuilder()
               .setNumber(2).setHash("hash2").setUrl("url2").build();

      // Construct a blocksListResponse object.
      final Athena.BlocksListResponse blocksListResponseObj = Athena.BlocksListResponse
               .newBuilder().addBlocks(blockBriefObj1).addBlocks(blockBriefObj2)
               .setNext("next").build();

      // Envelope the blocksListResponse object into an athena object.
      final Athena.AthenaResponse athenaresponseObj = Athena.AthenaResponse
               .newBuilder().setBlocksListResponse(blocksListResponseObj)
               .build();

      return athenaresponseObj;
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

      // Read list of blocks from the blocks list response object.
      List<Athena.BlockBrief> blockList = new ArrayList<>();
      blockList = blocksListResponse.getBlocksList();

      JSONArray blockArr = new JSONArray();

      // Iterate through each block and construct a corresponding JSON object
      for (Athena.BlockBrief block : blockList) {
         JSONObject blockJson = new JSONObject();
         blockJson.put("number", Long.toString(block.getNumber()));
         blockJson.put("hash", block.getHash());
         blockJson.put("url", block.getUrl());

         // Store into a JSON array of all blocks.
         blockArr.add(blockJson);
      }

      // Construct the reponse JSON object.
      JSONObject responseJson = new JSONObject();
      responseJson.put("blocks", blockArr);
      responseJson.put("next", blocksListResponse.getNext());

      return responseJson;
   }
}
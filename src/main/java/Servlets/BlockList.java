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

import connections.AthenaConnectionPool;
import connections.IAthenaConnection;

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
public final class BlockList extends BaseServlet {
   private static final long serialVersionUID = 1L;
   private final static Logger logger = Logger.getLogger(BlockList.class);

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

      // Read the request params
      Long end = null;
      Long listLength = null;

      try {
         end = Long.parseLong(request.getParameter("end"));

      } catch (Exception e) {
         // Do nothing as this parameter is optional
      }

      try {
         listLength = Long.parseLong(request.getParameter("listLength"));
      } catch (Exception e) {
         // Do nothing as this parameter is optional
      }

      // Construct a blocksListRequest object.
      Athena.BlocksListRequest.Builder b = Athena.BlocksListRequest
               .newBuilder();
      // If end is null, Athena assumes end is the latest block
      if (end != null) {
         b.setEnd(end);
      }

      // If listLength is null, request for default no. of blocks
      if (listLength == null) {
         listLength = _conf
                     .getLongValue("BlockList_DefaultListLength");
      }
      b.setListLength(listLength);

      Athena.BlocksListRequest blocksListRequestObj =
         b.build();

      // Envelope the blocksListRequest object into an athena object.
      final Athena.AthenaRequest athenarequestObj =
         Athena.AthenaRequest.newBuilder()
            .setBlocksListRequest(blocksListRequestObj)
            .build();

      processGet(athenarequestObj, response, logger);
   }

   /**
    * Note : This is a temporary function which mocks Athena's response.
    *
    * @return
   */
   public JSONObject receiveFromAthenaMock() {
      final Athena.BlockBrief blockBriefObj1 =
            Athena.BlockBrief.newBuilder()
            .setNumber(1).
            setHash("hash1").
            setUrl("url1").
            build();
      final Athena.BlockBrief blockBriefObj2 =
            Athena.BlockBrief.newBuilder()
            .setNumber(2)
            .setHash("hash2")
            .setUrl("url2")
            .build();

      // Construct a blocksListResponse object.
      final Athena.BlocksListResponse blocksListResponseObj =
            Athena.BlocksListResponse.newBuilder()
            .addBlocks(blockBriefObj1)
            .addBlocks(blockBriefObj2)
            .setNext("next")
            .build();

      // Envelope the blocksListResponse object into an athena object.
      final Athena.AthenaResponse athenaresponseObj =
            Athena.AthenaResponse.newBuilder()
            .setBlocksListResponse(blocksListResponseObj)
            .build();

      return parseToJSON(athenaresponseObj);
   }

   /**
    * Parses the Protocol Buffer response from Athena
    * and converts it into JSON.
    * @param athenaResponse
    *           Protocol Buffer object containing Athena's reponse
    * @return Response in JSON format
   */
   @SuppressWarnings("unchecked")
   @Override
   protected JSONObject parseToJSON(
         Athena.AthenaResponse athenaResponse) {
      try {
         // Extract the blocklist response
         // from the athena reponse envelope.
         Athena.BlocksListResponse blocksListResponse = athenaResponse
               .getBlocksListResponse();

         // Read list of blocks from the blocks list response object.
         List<Athena.BlockBrief> blockList = new ArrayList<>();
         blockList = blocksListResponse.getBlocksList();

         JSONArray blockArr = new JSONArray();

         // Iterate through each block
         // and construct a corresponding JSON object
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
      } catch (Exception e) {
         logger.error("parseToJson", e);
         return null;
      }
   }
}

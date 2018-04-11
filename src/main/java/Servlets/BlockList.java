/**
 * url endpoint : /api/athena/blocks
 *
 * Used to list blocks in the chain, most recent first.
 *
 * This servlet is used to send BlockList Requests to Athena and to parse the
 * responses into JSON. A TCP socket connection is made to Athena and requests
 * and responses are encoded in the Google Protocol Buffer format.
 *
 */
package Servlets;

import com.google.protobuf.ByteString;
import com.vmware.athena.*;

import java.io.IOException;
import java.util.ArrayList;
import java.util.List;
import java.util.Random;

import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import org.apache.log4j.Logger;
import org.json.simple.JSONArray;
import org.json.simple.JSONObject;

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
      Athena.BlocksListRequest.Builder b
         = Athena.BlocksListRequest.newBuilder();
      // If end is null, Athena assumes end is the latest block
      if (latest != null) {
         b.setLatest(latest);
      }

      // If listLength is null, request for default no. of blocks
      if (count == null) {
         count = _conf.getLongValue("BlockList_DefaultCount");
      }
      b.setCount(count);

      Athena.BlocksListRequest blocksListRequestObj = b.build();

      // Envelope the blocksListRequest object into an athena object.
      final Athena.AthenaRequest athenarequestObj
         = Athena.AthenaRequest.newBuilder()
                               .setBlocksListRequest(blocksListRequestObj)
                               .build();

      /////////////////// This is temporary.//////////////////////
      Athena.AthenaResponse athenaResponse = null;
      JSONObject blocksListResponse = null;

      if (latest == null) {
         latest = -1L;
      }
      athenaResponse = receiveFromAthenaMock(count, latest);
      blocksListResponse = parseToJSON(athenaResponse);

      // Set client response header
      response.setHeader("Content-Transfer-Encoding", "UTF-8");
      response.setContentType("application/json");
      response.getWriter().write(blocksListResponse.toString());
      ///////////////////////////////////////////////////////////

      // Coming soon
      // processGet(athenarequestObj, response, logger);
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

      // -1 means latest was not specified
      if (latestBlock == -1L) {
         latestBlock = 100;
      }

      // we're simiulating a total of 100 blocks, cap to that
      if (latestBlock > 100) {
         latestBlock = 100;
      }

      if (chainLength > latestBlock) {
         // need the +1 because blocks start at 0
         chainLength = latestBlock+1;
      }

      ArrayList<FakeBlock> list = new ArrayList<>(chainLength);

      for (int i = chainLength-1; i >= 0; i--) {
         list.add(new FakeBlock(latestBlock));
         latestBlock--;
      }

      Athena.BlocksListResponse.Builder b
         = Athena.BlocksListResponse.newBuilder();

      for (FakeBlock f : list) {
         final Athena.BlockBrief blockBriefObj
            = Athena.BlockBrief.newBuilder()
                               .setNumber(f.number)
                               .setHash(f.hash)
                               .build();
         b.addBlocks(blockBriefObj);
      }

      final Athena.BlocksListResponse r = b.build();
      Athena.AthenaResponse athenaResponseObj
         = Athena.AthenaResponse.newBuilder().setBlocksListResponse(r).build();

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
   @Override
   protected JSONObject parseToJSON(Athena.AthenaResponse athenaResponse) {
      try {
         // Extract the blocklist response
         // from the athena reponse envelope.
         Athena.BlocksListResponse blocksListResponse
            = athenaResponse.getBlocksListResponse();

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

            blockJson.put("url",
                          _conf.getStringValue("BlockList_URLPrefix") + url);

            // Store into a JSON array of all blocks.
            blockArr.add(blockJson);
            earliestBlock = number;
         }

         // Construct the reponse JSON object.
         JSONObject responseJson = new JSONObject();
         responseJson.put("blocks", blockArr);
         if (earliestBlock > 0) {
            responseJson.put("next",
                             _conf.getStringValue("BlockList_NextPrefix")
                                + (earliestBlock - 1));
         }

         return responseJson;
      } catch (Exception e) {
         logger.error("parseToJson", e);
         return null;
      }
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
            logger.error("Error converting from hex string to binary string");
            byte[] temp = new byte[32];
            hash = ByteString.copyFrom(temp);
         }
      }
   }
}

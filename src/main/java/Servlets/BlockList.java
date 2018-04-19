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
import org.json.simple.JSONAware;

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
      Athena.BlockListRequest.Builder b
         = Athena.BlockListRequest.newBuilder();
      // If end is null, Athena assumes end is the latest block
      if (latest != null) {
         b.setLatest(latest);
      }

      // If listLength is null, request for default no. of blocks
      if (count == null) {
         count = _conf.getLongValue("BlockList_DefaultCount");
      }
      b.setCount(count);

      Athena.BlockListRequest blocksListRequestObj = b.build();

      // Envelope the blocksListRequest object into an athena object.
      final Athena.AthenaRequest athenarequestObj
         = Athena.AthenaRequest.newBuilder()
                               .setBlockListRequest(blocksListRequestObj)
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
   protected JSONAware parseToJSON(Athena.AthenaResponse athenaResponse) {
      try {
         // Extract the blocklist response
         // from the athena reponse envelope.
         Athena.BlockListResponse blockListResponse
            = athenaResponse.getBlockListResponse();

         long earliestBlock = -1L;

         // Read list of blocks from the blocks list response object.
         List<Athena.BlockBrief> blockList = new ArrayList<>();
         blockList = blockListResponse.getBlockList();

         JSONArray blockArr = new JSONArray();

         // Iterate through each block and construct a corresponding JSON object
         for (Athena.BlockBrief block : blockList) {
            JSONObject blockJson = new JSONObject();

            long number = block.getNumber();
            String hexString = APIHelper.binaryStringToHex(block.getHash());

            blockJson.put("number", number);
            blockJson.put("hash", hexString);

            String url = _conf.getStringValue("BlockList_URLPrefix") + number;

            blockJson.put("url", url);

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
}

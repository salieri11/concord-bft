package Servlets;

import java.util.ArrayList;
import java.util.List;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.json.simple.JSONArray;
import org.json.simple.JSONAware;
import org.json.simple.JSONObject;
import org.springframework.http.ResponseEntity;
import org.springframework.stereotype.Controller;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestMethod;
import org.springframework.web.bind.annotation.RequestParam;

import com.vmware.athena.Athena;

@Controller
public class BlockList extends BaseServlet {

   private static final long serialVersionUID = 1L;

   private Logger logger = LogManager.getLogger(BlockList.class);

   // ** - tells spring to match anything in path
   @RequestMapping(method = RequestMethod.GET,
           path = "/api/athena/blocks")
   public ResponseEntity<JSONAware>
             getBlockList(@RequestParam(name = "latest", defaultValue = "-1",
                                        required = false) long latest,
                          @RequestParam(name = "count", required = false,
                                        defaultValue = "-1") long count) {
      // return ResponseEntity.ok(new JSONObject());

      // Construct a blocksListRequest object.
      Athena.BlockListRequest.Builder b = Athena.BlockListRequest.newBuilder();
      // If end is null, Athena assumes end is the latest block
      if (latest != -1) {
         b.setLatest(latest);
      }

      // If listLength is null, request for default no. of blocks
      if (count == -1) {
         count = _conf.getLongValue("BlockList_DefaultCount");
      }
      b.setCount(count);

      Athena.BlockListRequest blocksListRequestObj = b.build();

      // Envelope the blocksListRequest object into an athena object.
      final Athena.AthenaRequest athenarequestObj
         = Athena.AthenaRequest.newBuilder()
                               .setBlockListRequest(blocksListRequestObj)
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

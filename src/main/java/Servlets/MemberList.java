/**
 * url endpoint : /api/athena/members
 * Used to fetch the Athena Consensus Membership List.
 *
 * This servlet is used to send Peer Requests to Athena and to parse
 * the responses into JSON. A TCP socket connection is made to Athena
 * and requests and responses are encoded in the Google Protocol Buffer
 * format.
 *
 * TODO : Handle the case of no/incorrect response from Athena
 */
package Servlets;

import com.vmware.athena.*;
import connections.AthenaConnectionPool;
import connections.IAthenaCommunication;
import connections.IAthenaConnection;

import java.io.IOException;
import java.util.ArrayList;
import java.util.List;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import org.apache.log4j.Logger;
import org.json.simple.JSONArray;
import org.json.simple.JSONObject;

/**
 * Servlet class.
 */
public final class MemberList extends BaseServlet {
   private static final long serialVersionUID = 1L;
   private static final Logger logger = Logger.getLogger(MemberList.class);

   /**
    * Services a get request. Constructs a protobuf request of type peer request
    * (enveloped in an athena request) as defined in athena.proto. Sends this
    * request to Athena. Parses the response and converts it into json for
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
      // Construct a peer request object. Set its return_peers field.
      final Athena.PeerRequest peerRequestObj = Athena.PeerRequest.newBuilder()
               .setReturnPeers(true).build();

      // Envelope the peer request object into an athena object.
      final Athena.AthenaRequest athenarequestObj = Athena.AthenaRequest
               .newBuilder().setPeerRequest(peerRequestObj).build();

      processGet(athenarequestObj, response, logger);
   }

   @Override
   protected void processGet(Athena.AthenaRequest req,
            HttpServletResponse response, Logger log) {
      JSONArray respObject = null;
      IAthenaConnection conn = null;
      Athena.AthenaResponse athenaResponse = null;
      try {
         conn = AthenaConnectionPool.getInstance().getConnection();
         boolean res = AthenaHelper.sendToAthena(req, conn, _conf);
         if (!res) {
            processResponse(response, "Communication error",
                     HttpServletResponse.SC_INTERNAL_SERVER_ERROR, log);
            return;
         }

         // receive response from Athena
         athenaResponse = AthenaHelper.receiveFromAthena(conn);
         if (athenaResponse == null) {
            processResponse(response, "Data error",
                     HttpServletResponse.SC_INTERNAL_SERVER_ERROR, log);
            return;
         }
      } catch (Exception e) {
         processResponse(response, "Internal error",
                  HttpServletResponse.SC_INTERNAL_SERVER_ERROR, log);
         return;
      } finally {
         AthenaConnectionPool.getInstance().putConnection(conn);
      }

      respObject = parseToJSONArray(athenaResponse);
      String json = respObject == null ? null : respObject.toJSONString();

      processResponse(response, json,
               json == null ? HttpServletResponse.SC_INTERNAL_SERVER_ERROR
                        : HttpServletResponse.SC_OK,
               log);
   }

   /**
    * Parses the Protocol Buffer response from Athena and converts
    * it into JSON.
    * Method overridden because the response for this API is of type
    * JSONArray
    * as opposed to JSONObject.
    *
    * @param athenaResponse
    *           Protocol Buffer object containing Athena's reponse
    * @return Response in JSON format
    */
   @SuppressWarnings("unchecked")
   protected JSONArray parseToJSONArray(
         Athena.AthenaResponse athenaResponse) {
      // Extract the peer response from the athena reponse envelope.
      Athena.PeerResponse peerResponse = athenaResponse.getPeerResponse();

      // Read list of peer objects from the peer response object.
      List<Athena.Peer> peerList = new ArrayList<>();
      peerList = peerResponse.getPeerList();

      JSONArray peerArr = new JSONArray();

      // Iterate through each peer and construct
      // a corresponding JSON object
      for (Athena.Peer peer : peerList) {
         JSONObject peerJson = new JSONObject();
         peerJson.put("address", peer.getAddress());
         peerJson.put("port", peer.getPort());
         peerJson.put("status", peer.getStatus());

         // Store into a JSON array of all peers.
         peerArr.add(peerJson);
      }

      return peerArr;
   }

   @Override
   protected JSONObject parseToJSON(Athena.AthenaResponse athenaResponse) {
      return null;
   }
}

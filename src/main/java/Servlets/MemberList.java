/**
* url endpoint : /api/athena/members
 * Used to fetch the Athena Consensus Membership List.
 * 
 * This servlet is used to send Peer Requests to Athena and to parse
 * the responses into JSON. A TCP socket connection is made to Athena
 * and requests and responses are encoded in the Google Protocol Buffer
 * format.
 * 
 * Athena, by default, runs on port 5458.
 * 
 */
package Servlets;

import com.google.protobuf.InvalidProtocolBufferException;
import com.vmware.athena.*;

import connections.AthenaConnectionPool;
import connections.AthenaTCPConnection;
import connections.IAthenaConnection;

import java.io.DataInputStream;
import java.io.DataOutputStream;
import java.io.IOException;
import java.io.PrintWriter;
import java.util.ArrayList;
import java.util.List;
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
      final Athena.PeerRequest peerRequestObj = 
            Athena.PeerRequest.newBuilder()
            .setReturnPeers(true)
            .build();

      // Envelope the peer request object into an athena object.
      final Athena.AthenaRequest athenarequestObj = Athena.AthenaRequest
            .newBuilder()
            .setPeerRequest(peerRequestObj)
            .build();

      processGet(athenarequestObj, response, logger);
   }

   /**
    * Parses the Protocol Buffer response from Athena
    * and converts it into JSON.
    * 
    * @param athenaResponse
    *           Protocol Buffer object containing Athena's reponse
    * @return Response in JSON format
    */
   @SuppressWarnings("unchecked")
   @Override
   protected JSONObject parseToJSON(
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
         peerJson.put("port", Integer.toString(peer.getPort()));
         peerJson.put("status", peer.getStatus());

         // Store into a JSON array of all peers.
         peerArr.add(peerJson);
      }

      // Construct the reponse JSON object.
      JSONObject responseJson = new JSONObject();
      responseJson.put("peer_response", peerArr);

      return responseJson;
   }
}

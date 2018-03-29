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
 * TODO : Handle the case of no/incorrect response from Athena
 */
package Servlets;

import com.vmware.athena.*;

import connections.AthenaTCPConnection;

import java.io.IOException;
import java.io.PrintWriter;
import java.util.ArrayList;
import java.util.List;

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
public final class MemberList extends HttpServlet {
   private static final long serialVersionUID = 1L;
   private static AthenaTCPConnection athenaConnection;
   private final Logger logger;

   /**
    * Retrieves the common TCP connection object.
    * 
    * @throws IOException
    * @throws ParseException
    */
   public MemberList() throws IOException, ParseException {
      logger = Logger.getLogger(MemberList.class);
      try {
         athenaConnection = AthenaTCPConnection.getInstance();
      } catch (IOException e) {
         logger.error("Error in creating TCP connection with Athena");
         throw e;
      }
   }

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
      PrintWriter writer = null;
      try {
         writer = response.getWriter();
      } catch (IOException e) {
         logger.error("Error in retrieving the writer object of the "
                  + "HttpResponse");
         throw e;
      }

      // Construct a peer request object. Set its return_peers field.
      final Athena.PeerRequest peerRequestObj = Athena.PeerRequest.newBuilder()
               .setReturnPeers(true).build();

      // Envelope the peer request object into an athena object.
      final Athena.AthenaRequest athenarequestObj = Athena.AthenaRequest
               .newBuilder().setPeerRequest(peerRequestObj).build();

      // send request to Athena and receive response
      Athena.AthenaResponse athenaResponse = athenaConnection.sendToAthena(athenarequestObj);

      // Convert Protocol Buffer to JSON.
      JSONArray peerResponse = parseToJSON(athenaResponse);

      // Set client response header
      response.setHeader("Content-Transfer-Encoding", "UTF-8");
      response.setContentType("application/json");

      // Respond to client.
      writer.write(peerResponse.toString());
   }

   /**
    * Parses the Protocol Buffer response from Athena and converts it into JSON.
    * 
    * @param athenaResponse
    *           Protocol Buffer object containing Athena's reponse
    * @return Response in JSON format
    */
   @SuppressWarnings("unchecked")
   private JSONArray parseToJSON(Athena.AthenaResponse athenaResponse) {

      // Extract the peer response from the athena reponse envelope.
      Athena.PeerResponse peerResponse = athenaResponse.getPeerResponse();

      // Read list of peer objects from the peer response object.
      List<Athena.Peer> peerList = new ArrayList<>();
      peerList = peerResponse.getPeerList();

      JSONArray peerArr = new JSONArray();

      // Iterate through each peer and construct a corresponding JSON object
      for (Athena.Peer peer : peerList) {
         JSONObject peerJson = new JSONObject();
         peerJson.put("address", peer.getAddress());
         peerJson.put("port", peer.getPort());
         peerJson.put("status", peer.getStatus());

         // Store into a JSON array of all peers.
         peerArr.add(peerJson);
      }

      // Construct the reponse JSON.
      return peerArr;
   }
}
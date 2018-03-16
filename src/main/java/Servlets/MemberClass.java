/**
 * url endpoint : /api/athena/members
 * 
 * This servlet is used to send Peer Requests to Athena and to parse
 * the responses into JSON. A TCP socket connection is made to Athena
 * and requests and responses are encoded in the Google Protocol Buffer
 * format.
 * 
 * Athena, by default, runs on port 5458.
 * TODO : Read Athena port from config file.
 * 
 */
package Servlets;

import com.vmware.athena.*;
import java.io.DataInputStream;
import java.io.DataOutputStream;
import java.io.IOException;
import java.io.PrintWriter;
import java.net.Socket;
import java.util.ArrayList;
import java.util.List;

import javax.servlet.http.HttpServlet;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import org.json.simple.JSONArray;
import org.json.simple.JSONObject;

/**
 * Servlet class.
 */
public class MemberClass extends HttpServlet {
    private static final long serialVersionUID = 1L;
    private static final String HOST_NAME = "localhost";
    private static final int PORT = 5458;

    /**
     * Services a get request. Constructs a protobuf request of type peer
     * request (enveloped in an athena request) as defined in athena.proto.
     * Sends this request to Athena. Parses the response and converts it into
     * json for responding to the client.
     * 
     * @param request
     *            The request received by the servlet
     * @param response
     *            The response object used to respond to the client
     */
    @Override
    protected void doGet(final HttpServletRequest request,
            final HttpServletResponse response) {
        PrintWriter writer = null;
        try {
            writer = response.getWriter();
        } catch (IOException e) {
            e.printStackTrace();
        }

        // Construct a peer request object. Set its return_peers field.
        final Athena.PeerRequest peerRequestObj = Athena.PeerRequest
                .newBuilder().setReturnPeers(true).build();

        // Envelope the peer request object into an athena object.
        final Athena.AthenaRequest athenarequestObj = Athena.AthenaRequest
                .newBuilder().setPeerRequest(peerRequestObj).build();

        Socket athenaSocket = null;
        DataOutputStream outToServer = null;
        DataInputStream inFromServer = null;

        try {
            // Open a TCP socket connection with Athena.
            athenaSocket = openConnection(HOST_NAME, PORT);

            // Build outputstream object and use it to send request to Athena
            outToServer = new DataOutputStream(athenaSocket.getOutputStream());
            sendToAthena(outToServer, athenarequestObj);

            // Build inputstream obj and use it to receive response from Athena
            inFromServer = new DataInputStream(athenaSocket.getInputStream());
            JSONObject peerResponse = receiveFromAthena(inFromServer);

            // Set client response header
            response.setHeader("Content-Transfer-Encoding", "UTF-8");
            response.setContentType("text/html");

            // Respond to client.
            writer.write(peerResponse.toString());
        } catch (Exception e) {
            e.printStackTrace();
        } finally {
            // Close the TCP Socket connection with Athena.
            closeConnection(athenaSocket, outToServer, inFromServer);
        }
    }

    /**
     * Opens a TCP Socket connection with Athena.
     * 
     * @param hostName
     *            Hostname of a running Athena instance
     * @param port
     *            Port number of a running Athena instance
     * @return Socket object of the connection
     */
    private Socket openConnection(final String hostName, final int port) {
        Socket athenaSocket = null;
        try {
            athenaSocket = new Socket(HOST_NAME, PORT);
            System.out.println("Socket connection with Athena created");
        } catch (IOException e) {
            e.printStackTrace();
        }
        return athenaSocket;
    }

    /**
     * Cleans up resources. Closes the inputstream, outputstream and TCP socket
     * connection with Athena.
     * 
     * @param socket
     *            Socket connection
     * @param socketRequest
     *            OutputStream object
     * @param socketResponse
     *            InputStream object
     */
    private void closeConnection(Socket socket, DataOutputStream socketRequest,
            DataInputStream socketResponse) {
        if (socketRequest != null) {
            try {
                socketRequest.close();
            } catch (IOException e) {
                e.printStackTrace();
            }
        }
        if (socketResponse != null) {
            try {
                socketResponse.close();
            } catch (IOException e) {
                e.printStackTrace();
            }
        }
        if (socket != null) {
            try {
                socket.close();
            } catch (IOException e) {
                e.printStackTrace();
            }
        }
    }

    /**
     * Sends a Google Protocol Buffer request to Athena. Athena expects two
     * bytes signifying the size of the request before the actual request.
     * 
     * @param socketRequest
     *            OutputStream object
     * @param request
     *            AthenaRequest object
     */
    public void sendToAthena(DataOutputStream socketRequest,
            Athena.AthenaRequest request) {
        try {
            System.out.print("Sending request to Athena :");
            System.out.println(request);

            // Find size of request and pack size into two bytes.
            int requestSize = request.getSerializedSize();
            byte[] size = intToSizeBytes(requestSize);

            byte[] protobufRequest = request.toByteArray();

            // Write requests over the output stream.
            socketRequest.write(size);
            socketRequest.write(protobufRequest);
        } catch (IOException ex) {
            ex.printStackTrace();
        }
    }

    /**
     * Receives a Google Protocol Buffer response from Athena. Athena sends
     * two bytes signifying the size of the response before the actual 
     * response.
     * @param socketResponse InputStream object
     * @return Athena's response in JSON format
     */
    public JSONObject receiveFromAthena(DataInputStream socketResponse) {
        try {
            /*Read two bytes from the inputstream and consider that as 
             size of the response*/
            byte[] size = new byte[2];
            size[0] = socketResponse.readByte();
            size[1] = socketResponse.readByte();
            int responseSize = sizeBytesToInt(size);
            
            //Read the response from the input stream.
            byte[] response = new byte[responseSize];
            for (int i = 0; i < responseSize; i++) {
                response[i] = socketResponse.readByte();
            }
            
            //Convert read bytes into a Protocol Buffer object.
            Athena.AthenaResponse athenaResponse = Athena.AthenaResponse
                    .parseFrom(response);
            
            //Convert Protocol Buffer to JSON.
            JSONObject responseJson = parseToJSON(athenaResponse);
            
            return responseJson;
        } catch (IOException ex) {
            ex.printStackTrace();
        }
        return null;
    }

    /**
     * Parses the Protocol Buffer response from Athena and converts it into
     * JSON.
     * @param athenaResponse Protocol Buffer object containing Athena's reponse
     * @return Response in JSON format
     */
    @SuppressWarnings("unchecked")
    private JSONObject parseToJSON(Athena.AthenaResponse athenaResponse) {
        
        //Extract the peer response from the athena reponse envelope.
        Athena.PeerResponse peerResponse = athenaResponse.getPeerResponse();
        
        //Read list of peer objects from the peer response object.
        List<Athena.Peer> peerList = new ArrayList<>();
        peerList = peerResponse.getPeerList();

        JSONArray peerArr = new JSONArray();
        
        //Iterate through each peer and construct a corresponding JSON object
        for (Athena.Peer peer : peerList) {
            JSONObject peerJson = new JSONObject();
            peerJson.put("address", peer.getAddress());
            peerJson.put("port", Integer.toString(peer.getPort()));
            peerJson.put("status", peer.getStatus());
            
            //Store into a JSON array of all peers.
            peerArr.add(peerJson);
        }

        //Construct the reponse JSON object.
        JSONObject responseJson = new JSONObject();
        responseJson.put("peer_response", peerArr);

        return responseJson;
    }

    /**
     * Converts size in two bytes into a single int.
     * @param size Byte array containing two bytes of size
     * @return Size in int
     */
    private int sizeBytesToInt(byte[] size) {
        return ((size[1] & 0xff) << 8) | (size[0] & 0xff);
    }

    /**
     * Converts an int into two bytes.
     * @param a Integer that needs to be converted
     * @return A byte array containing two bytes.
     */
    public static byte[] intToSizeBytes(int a) {
        byte[] data = new byte[2];
        data[0] = (byte) (a & 0xFF);
        data[1] = (byte) ((a >> 8) & 0xFF);
        return data;
    }
}
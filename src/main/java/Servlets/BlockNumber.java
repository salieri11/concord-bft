/**
 * url endpoint : /api/athena/blocks/{N}
 * Used to fetch a specific block from the chain.
 * 
 * This servlet is used to send BlockNumber Requests to Athena and to parse
 * the responses into JSON. A TCP socket connection is made to Athena
 * and requests and responses are encoded in the Google Protocol Buffer
 * format.
 * 
 * Athena, by default, runs on port 5458.
 * TODO : Handle the case of no/incorrect response from Athena
 */
package Servlets;

import com.google.protobuf.InvalidProtocolBufferException;
import com.google.protobuf.ProtocolStringList;
import com.vmware.athena.*;

import connections.AthenaTCPConnection;
import io.undertow.util.StatusCodes;

import java.io.DataInputStream;
import java.io.DataOutputStream;
import java.io.IOException;
import java.io.PrintWriter;
import java.net.URLDecoder;
import java.util.Iterator;
import java.util.concurrent.locks.ReentrantLock;

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
public final class BlockNumber extends HttpServlet {
   private static final long serialVersionUID = 1L;
   private static DataOutputStream outToAthena;
   private static DataInputStream inFromAthena;
   private static ReentrantLock tcpLock;
   private final Logger logger;

   /**
    * Retrieves the common TCP connection object.
    * 
    * @throws IOException
    * @throws ParseException
    */
   public BlockNumber() throws IOException, ParseException {
      logger = Logger.getLogger(BlockNumber.class);
      AthenaTCPConnection athenaConnection = null;
      try {
         athenaConnection = AthenaTCPConnection.getInstance();
      } catch (IOException e) {
         logger.error("Error in creating TCP connection with Athena");
         throw new IOException();
      }
      outToAthena = athenaConnection.outputStream;
      inFromAthena = athenaConnection.inputStream;
      tcpLock = athenaConnection.tcpConnectionLock;
   }

   /**
    * Services a get request. Constructs a protobuf request of type blocknumber
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
         throw new IOException();
      }

      // Read the requested block number from the uri
      String index = null;
      try {
         String uri = request.getRequestURI();
         String urlParam = uri.substring(uri.lastIndexOf('/') + 1);

         // Remove HTTP encoding
         urlParam = URLDecoder.decode(urlParam, "UTF-8");

         // Remove opening and closing braces
         index = urlParam.substring(1, urlParam.length() - 1);
      } catch (NumberFormatException e) {
         logger.error("Invalid block number");
         response.sendError(StatusCodes.NOT_FOUND);
         throw new NumberFormatException();
      }

      // Construct a blockNumberRequest object. Set its start field.
      final Athena.BlockNumberRequest blockNumberRequestObj = Athena.BlockNumberRequest
               .newBuilder().setIndex(index).build();

      // Envelope the blockNumberRequest object into an athena object.
      final Athena.AthenaRequest athenarequestObj = Athena.AthenaRequest
               .newBuilder().setBlockNumberRequest(blockNumberRequestObj)
               .build();

      JSONObject blockNumberResponse = null;

      // Obtain a lock to allow only one thread to use the TCP connection at a
      // time
      tcpLock.lock();

      // try {
      // send request to Athena
      // sendToAthena(outToAthena, athenarequestObj); //Coming soon

      // receive response from Athena
      blockNumberResponse = receiveFromAthenaMock(); // This is temporary.
      // blockNumberResponse = receiveFromAthena(inFromAthena); //Coming soon

      // } catch (IOException e) {
      // logger.error("Error in communicating with Athena");
      // throw new IOException();
      // } finally {
      tcpLock.unlock();
      // }

      // Set client response header
      response.setHeader("Content-Transfer-Encoding", "UTF-8");
      response.setContentType("application/json");

      // Respond to client.
      writer.write(blockNumberResponse.toString());
   }

   /**
    * Sends a Google Protocol Buffer request to Athena. Athena expects two bytes
    * signifying the size of the request before the actual request.
    * 
    * @param socketRequest
    *           OutputStream object
    * @param request
    *           AthenaRequest object
    * @throws IOException
    */
   public void sendToAthena(DataOutputStream socketRequest,
            Athena.AthenaRequest request) throws IOException {

      // Find size of request and pack size into two bytes.
      int requestSize = request.getSerializedSize();
      byte[] size = intToSizeBytes(requestSize);

      byte[] protobufRequest = request.toByteArray();

      // Write requests over the output stream.
      try {
         socketRequest.write(size);
      } catch (IOException e) {
         logger.error("Error in writing the size of request to Athena");
         throw new IOException();
      }

      try {
         socketRequest.write(protobufRequest);
      } catch (IOException e) {
         logger.error("Error in writing the request to Athena");
         throw new IOException();
      }
   }

   /**
    * Receives a Google Protocol Buffer response from Athena. Athena sends two
    * bytes signifying the size of the response before the actual response.
    * 
    * @param socketResponse
    *           InputStream object
    * @return Athena's response in JSON format
    * @throws IOException
    */
   public JSONObject receiveFromAthena(DataInputStream socketResponse)
            throws IOException {
      /*
       * Read two bytes from the inputstream and consider that as size of the
       * response
       */
      byte[] size = new byte[2];
      try {
         socketResponse.readFully(size);
      } catch (IOException e) {
         logger.error("Error reading size of Athena's response");
         throw new IOException();
      }
      int responseSize = sizeBytesToInt(size);

      // Read the response from the input stream.
      byte[] response = new byte[responseSize];
      try {
         socketResponse.readFully(response);
      } catch (IOException e) {
         logger.error("Error reading Athena's response");
         throw new IOException();
      }

      // Convert read bytes into a Protocol Buffer object.
      Athena.AthenaResponse athenaResponse;
      try {
         athenaResponse = Athena.AthenaResponse.parseFrom(response);
      } catch (InvalidProtocolBufferException e) {
         logger.error("Error in parsing Athena's response");
         throw new InvalidProtocolBufferException(e.getMessage());
      }

      // Convert Protocol Buffer to JSON.
      JSONObject responseJson = parseToJSON(athenaResponse);
      return responseJson;
   }

   /**
    * Note : This is a temporary function which mocks Athena's response.
    * 
    * @return
    */
   public JSONObject receiveFromAthenaMock() {
      final Athena.BlockDetailed blockDetailedObj = Athena.BlockDetailed
               .newBuilder().setNumber(1).setHash("hash")
               .setParentHash("parentHash").setNonce("Nonce").setSize(50)
               .addTransactions("transaction1").addTransactions("transaction2")
               .build();

      // Construct a blockNumberResponse object.
      final Athena.BlockNumberResponse blockNumberResponseObj = Athena.BlockNumberResponse
               .newBuilder().setBlock(blockDetailedObj).build();

      // Envelope the blockNumberResponse object into an athena object.
      final Athena.AthenaResponse athenaresponseObj = Athena.AthenaResponse
               .newBuilder().setBlockNumberResponse(blockNumberResponseObj)
               .build();

      // Convert Protocol Buffer to JSON.
      JSONObject responseJson = parseToJSON(athenaresponseObj);
      return responseJson;
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

      // Extract the blocknumber response from the athena reponse envelope.
      Athena.BlockNumberResponse blockNumberResponse = athenaResponse
               .getBlockNumberResponse();

      // Read the block from the blocknumber response object.
      Athena.BlockDetailed block = blockNumberResponse.getBlock();

      JSONArray transactionArr = new JSONArray();
      ProtocolStringList transactionList = block.getTransactionsList();
      Iterator<String> it = transactionList.iterator();

      while (it.hasNext()) {
         transactionArr.add(it.next());
      }

      JSONObject blockObj = new JSONObject();
      blockObj.put("transactions", transactionArr);

      blockObj.put("number", block.getNumber());
      blockObj.put("hash", block.getHash());
      blockObj.put("parentHash", block.getParentHash());
      blockObj.put("nonce", block.getNonce());
      blockObj.put("size", block.getSize());

      // Construct the reponse JSON object.
      JSONObject responseJson = new JSONObject();
      responseJson.put("block", blockObj);

      return responseJson;
   }

   /**
    * Converts size in two bytes into a single int.
    * 
    * @param size
    *           Byte array containing two bytes of size
    * @return Size in int
    */
   private int sizeBytesToInt(byte[] size) {
      return ((size[1] & 0xff) << 8) | (size[0] & 0xff);
   }

   /**
    * Converts an int into two bytes.
    * 
    * @param a
    *           Integer that needs to be converted
    * @return A byte array containing two bytes.
    */
   public static byte[] intToSizeBytes(int a) {
      byte[] data = new byte[2];
      data[0] = (byte) (a & 0xFF);
      data[1] = (byte) ((a >> 8) & 0xFF);
      return data;
   }
}
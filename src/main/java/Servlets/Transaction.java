/**
 * url endpoint : /api/athena/transaction/{hash}
 * Used to get a specific transaction by its hash.
 * 
 * This servlet is used to send Transaction Requests to Athena and to parse
 * the responses into JSON. A TCP socket connection is made to Athena
 * and requests and responses are encoded in the Google Protocol Buffer
 * format.
 * 
 * Athena, by default, runs on port 5458.
 * TODO : Handle the case of no/incorrect response from Athena
 */
package Servlets;

import com.google.protobuf.InvalidProtocolBufferException;
import com.vmware.athena.*;

import connections.AthenaTCPConnection;
import io.undertow.util.StatusCodes;

import java.io.DataInputStream;
import java.io.DataOutputStream;
import java.io.IOException;
import java.io.PrintWriter;
import java.net.URLDecoder;
import java.util.concurrent.locks.ReentrantLock;

import javax.servlet.http.HttpServlet;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import org.apache.log4j.Logger;
import org.json.simple.JSONObject;
import org.json.simple.parser.ParseException;

/**
 * Servlet class.
 */
public final class Transaction extends HttpServlet {
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
   public Transaction() throws IOException, ParseException {
      logger = Logger.getLogger(Transaction.class);
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
    * Services a get request. Constructs a protobuf request of type transaction
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

      // Read the requested transaction hash from the uri
      String uri = request.getRequestURI();
      String hash = uri.substring(uri.lastIndexOf('/') + 1);
      hash = URLDecoder.decode(hash, "UTF-8");
      
      //Remove the opening and closing braces
      hash = hash.substring(1, hash.length() - 1);

      if (hash == null || hash.length() < 1) {
         logger.error("Empty hash in request");
         response.sendError(StatusCodes.BAD_REQUEST);
      }

      // Construct a transaction request object.
      final Athena.TransactionRequest txRequestObj = Athena.TransactionRequest
               .newBuilder().setHashParam(hash).build();

      // Envelope the transaction request object into an athena object.
      final Athena.AthenaRequest athenarequestObj = Athena.AthenaRequest
               .newBuilder().setTransactionRequest(txRequestObj).build();

      JSONObject txResponse = null;

      // Obtain a lock to allow only one thread to use the TCP connection at a
      // time
      tcpLock.lock();

      try {
         // send request to Athena
         sendToAthena(outToAthena, athenarequestObj);
         // receive response from Athena
         txResponse = receiveFromAthena(inFromAthena);
      } catch (IOException e) {
         logger.error("Error in communicating with Athena");
         throw new IOException();
      } finally {
         tcpLock.unlock();
      }

      // Set client response header
      response.setHeader("Content-Transfer-Encoding", "UTF-8");
      response.setContentType("application/json");

      // Respond to client.
      writer.write(txResponse.toString());
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
    * Parses the Protocol Buffer response from Athena and converts it into JSON.
    * 
    * @param athenaResponse
    *           Protocol Buffer object containing Athena's reponse
    * @return Response in JSON format
    */
   @SuppressWarnings("unchecked")
   private JSONObject parseToJSON(Athena.AthenaResponse athenaResponse) {

      // Extract the transaction response from the athena reponse envelope.
      Athena.TransactionResponse txResponse = athenaResponse
               .getTransactionResponse();

      // Construct the reponse JSON object.
      JSONObject responseJson = new JSONObject();
      responseJson.put("hash", txResponse.getHash());
      responseJson.put("from", txResponse.getFrom());
      responseJson.put("to", txResponse.getTo());
      responseJson.put("value", txResponse.getValue());
      responseJson.put("input", txResponse.getInput());
      responseJson.put("blockHash", txResponse.getBlockHash());
      responseJson.put("blockNumber", txResponse.getBlockNumber());
      responseJson.put("transactionHash", txResponse.getTransactionIndex());
      responseJson.put("blockNumber", txResponse.getBlockNumber());
      responseJson.put("transactionIndex", txResponse.getTransactionIndex());
      responseJson.put("nonce", txResponse.getNonce());

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
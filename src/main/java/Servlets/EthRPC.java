/**
 * url endpoint : /api/athena/eth
 *
 * GET:
 * Used to list available RPC methods.
 * A list of currently exposed Eth RPC methods is read from the config file
 * and returned to the client.
 * 
 * POST:
 * Used to call the named method.
 * An EthRPCExecute request is sent to Athena and to parse the responses into 
 * JSON. A TCP socket connection is made to Athena and requests and responses 
 * are encoded in the Google Protocol Buffer format.
 * 
 * Athena, by default, runs on port 5458.
 * TODO : Handle the case of no/incorrect response from Athena
 */
package Servlets;

import com.google.protobuf.InvalidProtocolBufferException;
import com.vmware.athena.*;
import com.vmware.athena.Athena.EthRPCExecuteResponse;

import connections.AthenaTCPConnection;
import io.undertow.util.StatusCodes;

import java.io.DataInputStream;
import java.io.DataOutputStream;
import java.io.IOException;
import java.io.PrintWriter;
import java.math.BigInteger;
import java.util.ArrayList;
import java.util.concurrent.locks.ReentrantLock;

import javax.servlet.http.HttpServlet;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import org.apache.log4j.Logger;
import org.json.simple.JSONArray;
import org.json.simple.JSONObject;
import org.json.simple.parser.JSONParser;
import org.json.simple.parser.ParseException;

/**
 * Servlet class.
 */
public final class EthRPC extends HttpServlet {
   private static final long serialVersionUID = 1L;
   private static DataOutputStream outToAthena;
   private static DataInputStream inFromAthena;
   private static ReentrantLock tcpLock;
   private final Logger logger;
   private static JSONArray rpcList;

   public EthRPC() {
      logger = Logger.getLogger(EthRPC.class);
   }

   /**
    * Services the Get request for listing currently exposed Eth RPCs. Retrieves
    * the list from the configurations file and returns it to the client.
    * 
    * @param request
    *           The request received by the servlet
    * @param response
    *           The response object used to respond to the client
    * @throws IOException
    */
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
      // Set client response header
      response.setHeader("Content-Transfer-Encoding", "UTF-8");
      response.setContentType("application/json");

      // Respond to client.
      writer.write(rpcList.toString());
   }

   /**
    * Services the Post request for executing the specified method. Retrieves
    * the request parameters and forwards the request to Athena for execution.
    * Receives a response from Athena and forwards it to the client.
    * 
    * @param request
    *           The request received by the servlet
    * @param response
    *           The response object used to respond to the client
    * @throws IOException
    */
   protected void doPost(final HttpServletRequest request,
            final HttpServletResponse response) throws IOException {

      // Make/fetch connection objects
      try {
         connectToAthena();
      } catch (ParseException e2) {
         logger.error("Error connecting to Athena");
         response.sendError(StatusCodes.INTERNAL_SERVER_ERROR);
      }

      PrintWriter writer = null;
      try {
         writer = response.getWriter();
      } catch (IOException e) {
         logger.error("Error in retrieving the writer object of the "
                  + "HttpResponse");
         throw new IOException();
      }

      // Retrieve the request fields
      JSONObject requestParams = null;
      Long id = null;
      String jsonRpc = null;
      String method = null;
      JSONArray params = null;

      JSONParser parser = new JSONParser();
      try {
         requestParams = (JSONObject) parser
                  .parse((String) request.getParameter("data"));
         if (requestParams == null) {
            logger.error("Invalid request");
            response.sendError(StatusCodes.BAD_REQUEST);
         }
      } catch (ParseException e1) {
         logger.error("Invalid request");
         response.sendError(StatusCodes.BAD_REQUEST);
      }

      try {
         id = (Long) requestParams.get("id");
      } catch (NumberFormatException e) {
         logger.error("Invalid request parameter : id");
         response.sendError(StatusCodes.BAD_REQUEST);
         throw new NumberFormatException();
      }

      jsonRpc = (String) requestParams.get("jsonrpc");
      if (jsonRpc == null) {
         logger.error("Invalid request parameter : jsonrpc");
         response.sendError(StatusCodes.BAD_REQUEST);
      }

      method = (String) requestParams.get("method");
      if (method == null) {
         logger.error("Invalid request parameter : method");
         response.sendError(StatusCodes.BAD_REQUEST);
      }

      params = (JSONArray) requestParams.get("params");

      if (params.size() < 1) {
         logger.error("Invalid request parameter : params");
         response.sendError(StatusCodes.BAD_REQUEST);
      }

      // Convert list of hex strings to list of binary strings
      // Athena expects params in binary strings
      ArrayList<String> paramBytes = hexStringToBinary(params);

      if (paramBytes == null) {
         logger.error("Invalid request parameter : params");
         response.sendError(StatusCodes.BAD_REQUEST);
      }

      // Construct an ethrpcexecute request object.
      Athena.EthRPCExecuteRequest ethRpcExecuteRequestObj = Athena.EthRPCExecuteRequest
               .newBuilder().setId(id).setJsonrpc(jsonRpc).setMethod(method)
               .addAllParams(paramBytes).build();

      // Envelope the request object into an athena request object.
      final Athena.AthenaRequest athenarequestObj = Athena.AthenaRequest
               .newBuilder().setEthRpcExecuteRequest(ethRpcExecuteRequestObj)
               .build();

      JSONObject ethRpcResponse = null;

      // Obtain a lock to allow only one thread to use the TCP connection at a
      // time
      tcpLock.lock();

      try {
         // send request to Athena
         sendToAthena(outToAthena, athenarequestObj);
         // receive response from Athena
         ethRpcResponse = receiveFromAthena(inFromAthena);
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
      writer.write(ethRpcResponse.toString());
   }

   /**
    * Retrieves the common TCP connection object and streams.
    * 
    * @throws IOException
    * @throws ParseException
    */
   private void connectToAthena() throws IOException, ParseException {
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

      // Extract the ethrpcexecute response from the athena reponse envelope.
      EthRPCExecuteResponse ethRpcExecuteResponse = athenaResponse
               .getEthRpcExecuteResponse();

      // Construct the response JSON object.
      JSONObject responseJson = new JSONObject();
      responseJson.put("id", ethRpcExecuteResponse.getId());
      responseJson.put("jsonrpc", ethRpcExecuteResponse.getJsonrpc());

      /*
       * Convert the binary string received from Athena into a hex string for
       * responding to the client
       */
      String resultString = binaryStringToHex(
               ethRpcExecuteResponse.getResult());
      responseJson.put("result", resultString);

      responseJson.put("error", ethRpcExecuteResponse.getError());
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

   /**
    * Converts list of hex strings into a list of binary strings.
    * 
    * @param params
    *           JSONArray of hex strings
    * @return
    */
   public static ArrayList<String> hexStringToBinary(JSONArray params) {
      ArrayList<String> result = new ArrayList<>();
      for (Object param : params) {
         String curr = ((String) param).substring(2); // Strip 0x from the start

         if (curr.length() % 2 != 0) {
            return null;
         }
         String byteConversion = new BigInteger(curr, 16).toString(2);
         int len = byteConversion.length();

         int padding = 8 - (len % 8);

         result.add(String.format("%" + (padding + len) + "s", byteConversion)
                  .replace(' ', '0'));
      }
      return result;
   }

   /**
    * Converts a binary string to a hex string.
    * 
    * @param binary
    *           Binary string
    * @return
    */
   public static String binaryStringToHex(String binary) {
      // TODO : Delete below line once Athena supports this request
      binary = "0000";

      int decimal = Integer.parseInt(binary, 2);
      String hex = Integer.toString(decimal, 16);
      StringBuilder sb = new StringBuilder("0x");
      sb.append(hex);
      return sb.toString();
   }
}

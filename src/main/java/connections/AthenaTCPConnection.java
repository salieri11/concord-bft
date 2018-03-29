/**
 * This singleton class is used to maintain resources related to a single
 * TCP connection with Athena.
 * 
 * Also contains functions for communicating with Athena over a TCP connection.
 * 
 * These resources are shared by all servlets.
 * This means that at present, only one servlet can talk to Athena at a
 * time.
 */
package connections;

import java.io.DataInputStream;
import java.io.DataOutputStream;
import java.io.IOException;
import java.net.Socket;
import java.net.UnknownHostException;
import java.util.Properties;

import org.apache.log4j.Logger;
import org.json.simple.parser.ParseException;

import com.google.protobuf.InvalidProtocolBufferException;
import com.vmware.athena.Athena;

import Servlets.APIHelper;
import configurations.SystemConfiguration;

public final class AthenaTCPConnection {

   private static AthenaTCPConnection single_instance = null;
   private Socket socket;
   public DataOutputStream outputStream;
   public DataInputStream inputStream;
   private static Logger logger;

   /**
    * Sets up a TCP connection with Athena and creates input and output streams
    * for this connection
    * 
    * @throws IOException
    * @throws ParseException
    */
   private AthenaTCPConnection() throws IOException, ParseException {
      logger = Logger.getLogger(AthenaTCPConnection.class);

      // Read Athena's hostname and port from the configurations file
      SystemConfiguration s = null;
      try {
         s = SystemConfiguration.getInstance();
      } catch (IOException e) {
         logger.error("Error in reading configurations");
         throw e;
      }
      Properties config = s.configurations;
      String athenaHostName = config.getProperty("AthenaHostName");
      int athenaPort = Integer.parseInt(config.getProperty("AthenaPort"));

      try {
         socket = new Socket(athenaHostName, athenaPort);
         outputStream = new DataOutputStream(socket.getOutputStream());
         inputStream = new DataInputStream(socket.getInputStream());
      } catch (UnknownHostException e) {
         logger.error("Error creating TCP connection with Athena");
         throw e;
      } catch (IOException e) {
         logger.error("Error creating input/output stream with Athena");
         throw e;
      }
      System.out.println("Socket connection with Athena created");
   }

   /**
    * This function ensures that only a single instance of this class is ever
    * created.
    * 
    * @return
    * @throws IOException
    * @throws ParseException
    */
   public static synchronized AthenaTCPConnection getInstance()
            throws IOException, ParseException {
      if (single_instance == null) {
         try {
            single_instance = new AthenaTCPConnection();
         } catch (IOException e) {
            logger.error("Error creating object of AthenaTCPConnection");
            throw e;
         }
      }
      return single_instance;
   }

   public static void closeConnection() throws IOException, ParseException {
      AthenaTCPConnection obj;
      try {
         obj = AthenaTCPConnection.getInstance();
      } catch (IOException e) {
         logger.error(
                  "Error in getting AthenaTCPConnection object for closing connection");
         throw e;
      }
      if (obj.outputStream != null) {
         try {
            {
               obj.outputStream.close();
            }
         } catch (IOException e) {
            logger.error("Error in closing output stream");
            throw e;
         }
      }
      if (obj.inputStream != null) {
         try {
            obj.inputStream.close();
         } catch (IOException e) {
            logger.error("Error in closing input stream");
            throw e;
         }
      }
      if (obj.socket != null) {
         try {
            obj.socket.close();
         } catch (IOException e) {
            logger.error("Error in closing TCP socket");
            throw e;
         }
      }
   }

   /**
    * Sends a Google Protocol Buffer request to Athena. Athena expects two bytes
    * signifying the size of the request before the actual request.
    * 
    * @param request
    *           The request that needs to be sent to Athena
    * @return Athena's response
    * @throws IOException
    */
   public synchronized Athena.AthenaResponse sendToAthena(
            Athena.AthenaRequest request) throws IOException {

      // Find size of request and pack size into two bytes.
      int requestSize = request.getSerializedSize();
      byte[] size = APIHelper.intToSizeBytes(requestSize);

      byte[] protobufRequest = request.toByteArray();

      // Write requests over the output stream.
      try {
         this.outputStream.write(size);
      } catch (IOException e) {
         logger.error("Error in writing the size of request to Athena");
         throw e;
      }

      try {
         this.outputStream.write(protobufRequest);
      } catch (IOException e) {
         logger.error("Error in writing the request to Athena");
         throw e;
      }

      // Get the response from Athena
      return receiveFromAthena();
   }

   /**
    * Receives a Google Protocol Buffer response from Athena. Athena sends two
    * bytes signifying the size of the response before the actual response.
    * 
    * @return Athena's response
    * @throws IOException
    */
   private Athena.AthenaResponse receiveFromAthena() throws IOException {
      /*
       * Read two bytes from the inputstream and consider that as size of the
       * response
       */
      byte[] size = new byte[2];
      try {
         this.inputStream.readFully(size);
      } catch (IOException e) {
         logger.error("Error reading size of Athena's response");
         throw e;
      }
      int responseSize = APIHelper.sizeBytesToInt(size);

      // Read the response from the input stream.
      byte[] response = new byte[responseSize];
      try {
         this.inputStream.readFully(response);
      } catch (IOException e) {
         logger.error("Error reading Athena's response");
         throw e;
      }

      // Convert read bytes into a Protocol Buffer object.
      Athena.AthenaResponse athenaResponse;
      try {
         athenaResponse = Athena.AthenaResponse.parseFrom(response);
      } catch (InvalidProtocolBufferException e) {
         logger.error("Error in parsing Athena's response");
         throw e;
      }
      return athenaResponse;
   }
}

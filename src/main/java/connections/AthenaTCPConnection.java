/**
 * This singleton class is used to maintain resources related to a single
 * TCP connection with Athena.
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
import java.util.concurrent.locks.ReentrantLock;

import org.apache.log4j.Logger;
import org.json.simple.parser.ParseException;

import configurations.SystemConfiguration;

public final class AthenaTCPConnection {

   private static AthenaTCPConnection single_instance = null;
   private Socket socket;

   public DataOutputStream outputStream;
   public DataInputStream inputStream;

   // Simple reentrant lock used to enforce that only one servlet can talk to
   // Athena at a time
   public ReentrantLock tcpConnectionLock;

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
      } catch (IOException e1) {
         logger.error("Error in reading configurations");
         throw new IOException();
      }
      Properties config = s.configurations;
      String athenaHostName = config.getProperty("AthenaHostName");
      int athenaPort = Integer.parseInt(config.getProperty("AthenaPort"));

      // Initialize the lock
      tcpConnectionLock = new ReentrantLock();

      // Create the TCP connection and input and output streams
      tcpConnectionLock.lock();
      try {
         socket = new Socket(athenaHostName, athenaPort);
         outputStream = new DataOutputStream(socket.getOutputStream());
         inputStream = new DataInputStream(socket.getInputStream());

      } catch (UnknownHostException e) {
         logger.error("Error creating TCP connection with Athena");
         throw new UnknownHostException();
      } catch (IOException e) {
         logger.error("Error creating input/output stream with Athena");
         throw new IOException();
      } finally {
         tcpConnectionLock.unlock();
      }
      System.out.println("Socket connection with Athena created");
   }

   public static AthenaTCPConnection getInstance() throws IOException, ParseException {
      if (single_instance == null) {
         try {
            single_instance = new AthenaTCPConnection();
         } catch (IOException e) {
            logger.error("Error creating object of AthenaTCPConnection");
            throw new IOException();
         }
      }
      return single_instance;
   }

   public static void closeConnection() throws IOException, ParseException {
      AthenaTCPConnection obj;
      try {
         obj = AthenaTCPConnection.getInstance();
      } catch (IOException e1) {
         logger.error(
                  "Error in getting AthenaTCPConnection object for closing connection");
         throw new IOException();
      }
      obj.tcpConnectionLock.lock();
      try {
         if (obj.outputStream != null) {
            try {
               {
                  obj.outputStream.close();
               }
            } catch (IOException e) {
               logger.error("Error in closing output stream");
               throw new IOException();
            }
         }
         if (obj.inputStream != null) {
            try {
               obj.inputStream.close();
            } catch (IOException e) {
               logger.error("Error in closing input stream");
               throw new IOException();
            }
         }
         if (obj.socket != null) {
            try {
               obj.socket.close();
            } catch (IOException e) {
               logger.error("Error in closing TCP socket");
               throw new IOException();
            }
         }
      } finally {
         obj.tcpConnectionLock.unlock();
      }
   }
}

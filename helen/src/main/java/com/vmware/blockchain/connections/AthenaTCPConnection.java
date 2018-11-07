/**
 * This class is used to maintain resources related to a TCP connection with
 * Athena. Also contains functions for communicating with Athena over a TCP
 * connection.
 *
 */
package com.vmware.blockchain.connections;

import java.io.IOException;
import java.net.Socket;
import java.net.UnknownHostException;
import java.nio.ByteBuffer;
import java.nio.ByteOrder;
import java.util.concurrent.atomic.AtomicBoolean;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

import com.vmware.athena.Athena;
import com.vmware.blockchain.common.AthenaProperties;
import com.vmware.blockchain.services.EthRPCHandlers.AthenaHelper;
import com.vmware.blockchain.services.EthRPCHandlers.EthDispatcher;

public final class AthenaTCPConnection implements IAthenaConnection {
   private Socket socket;
   private AtomicBoolean disposed;
   private final int receiveTimeout; // ms
   private final int receiveLengthSize; // bytes
   private AthenaProperties config;
   private static Logger logger
      = LogManager.getLogger(AthenaTCPConnection.class);
   private static Athena.ProtocolRequest _protocolRequestMsg
      = Athena.ProtocolRequest.newBuilder().setClientVersion(1).build();
   private static Athena.AthenaRequest _athenaRequest
      = Athena.AthenaRequest.newBuilder()
                            .setProtocolRequest(_protocolRequestMsg)
                            .build();

   /**
    * Sets up a TCP connection with Athena
    *
    * @throws IOException
    */
   public AthenaTCPConnection(AthenaProperties config, String host,
                              int port) throws IOException {
      this.config = config;
      receiveLengthSize = config.getReceiveHeaderSizeBytes();
      receiveTimeout = config.getReceiveTimeoutMs();
      disposed = new AtomicBoolean(false);

      // Create the TCP connection and input and output streams
      try {
         socket = new Socket(host, port);
         socket.setTcpNoDelay(true);
         socket.setSoTimeout(receiveTimeout);
      } catch (UnknownHostException e) {
         logger.error("Error creating TCP connection with Athena. Host= "
            + host + ", port= " + port);
         throw new UnknownHostException();
      } catch (IOException e) {
         logger.error("Error creating input/output stream with Athena. Host= "
            + host + ", port= " + port);
         throw new IOException();
      }

      logger.debug("Socket connection with Athena created");
   }

   /**
    * Closes the TCP connection
    */
   @Override
   public void close() {
      if (disposed.get())
         return;

      if (socket != null && !socket.isClosed()) {
         try {
            socket.close();
         } catch (IOException e) {
            logger.error("Error in closing TCP socket");
         } finally {
            disposed.set(true);
         }
      }
   }

   /**
    * Close the connection before garbage collection
    */
   @Override
   protected void finalize() throws Throwable {
      logger.info("connection disposed");
      try {
         if (!disposed.get())
            close();
      } finally {
         super.finalize();
      }
   }

   /**
    * Reads responses from Athena. Athena sends the size of the response before
    * the actual response
    */
   @Override
   public byte[] receive() {
      try {
         java.io.InputStream is = socket.getInputStream();
         long start = System.currentTimeMillis();
         int msgSize = -1;
         byte[] msgSizeBuf = new byte[receiveLengthSize];
         int msgSizeOffset = 0;
         byte[] result = null;
         int resultOffset = 0;

         while (System.currentTimeMillis() - start < receiveTimeout) {
            // we need to read at least the header before we can do anything
            if (msgSizeOffset < receiveLengthSize) {
               int count = is.read(msgSizeBuf,
                                   msgSizeOffset,
                                   receiveLengthSize - msgSizeOffset);
               if (count < 0) {
                  logger.error("No bytes read from athena");
                  break;
               } else {
                  msgSizeOffset += count;
               }
            }

            // we have the header - find out how big the body is
            if (msgSizeOffset == receiveLengthSize && msgSize < 0) {
               // msgSize is sent as an unsigned 16-bit integer
               msgSize
                  = Short.toUnsignedInt(ByteBuffer.wrap(msgSizeBuf)
                                                  .order(ByteOrder.LITTLE_ENDIAN)
                                                  .getShort());

               result = new byte[msgSize];
            }

            // now we can read the body
            if (result != null) {
               int count
                  = is.read(result, resultOffset, msgSize - resultOffset);
               if (count < 0) {
                  logger.error("No bytes read from athena");
                  break;
               } else {
                  resultOffset += count;
               }

               // stop when we've reached the end
               if (resultOffset == msgSize) {
                  break;
               }
            }
         }

         // if we didn't read the whole message, consider the stream corrupt and
         // close it
         if (resultOffset != msgSize) {
            logger.error("Failed to receive message (" + resultOffset + " != "
               + msgSize + "). Closing socket.");
            close();
            return null;
         }

         return result;
      } catch (IOException e) {
         logger.error("Failed to read from socket", e);
         close();
         return null;
      }
   }

   /**
    * Sends data to Athena over the connection
    */
   @Override
   public boolean send(byte[] msg) {
      try {
         socket.getOutputStream().write(msg);
         return true;
      } catch (Exception e) {
         logger.error("sendMessage", e);
         return false;
      }
   }

   @Override
   public boolean check() {
      try {
         logger.trace("check enter");
         boolean res = AthenaHelper.sendToAthena(_athenaRequest, this, config);
         if (res) {
            Athena.AthenaResponse resp = AthenaHelper.receiveFromAthena(this);
            if (resp != null) {
               Athena.ProtocolResponse pResp = resp.getProtocolResponse();
               if (pResp != null) {
                  logger.debug("check, got server version: "
                     + pResp.getServerVersion());
                  EthDispatcher.netVersion = pResp.getNetVersion();
                  return true;
               }
            }
         }

         return false;
      } catch (IOException e) {
         logger.error("check", e);
         return false;
      } finally {
         logger.trace("check exit");
      }
   }
}

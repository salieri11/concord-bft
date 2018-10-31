/**
 * This class is used to maintain resources related to a TCP connection with
 * Athena. Also contains functions for communicating with Athena over a TCP
 * connection.
 *
 */
package connections;

import java.io.IOException;
import java.net.Socket;
import java.net.UnknownHostException;
import java.nio.ByteBuffer;
import java.nio.ByteOrder;
import java.util.concurrent.atomic.AtomicBoolean;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

import com.vmware.athena.Athena;

import Servlets.AthenaHelper;
import Servlets.EthDispatcher;
import configurations.IConfiguration;

public final class AthenaTCPConnection implements IAthenaConnection {
   private Socket _socket;
   private AtomicBoolean _disposed;
   private final int _receiveTimeout; // ms
   private final int _receiveLengthSize; // bytes
   private IConfiguration _conf;
   private static Logger _logger
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
   public AthenaTCPConnection(IConfiguration conf, String host,
                              int port) throws IOException {
      _conf = conf;
      _receiveLengthSize = _conf.getIntegerValue("ReceiveHeaderSizeBytes");
      _receiveTimeout = _conf.getIntegerValue("ReceiveTimeoutMs");
      _disposed = new AtomicBoolean(false);

      // Create the TCP connection and input and output streams
      try {
         _socket = new Socket(host, port);
         _socket.setTcpNoDelay(true);
         _socket.setSoTimeout(_receiveTimeout);
      } catch (UnknownHostException e) {
         _logger.error("Error creating TCP connection with Athena. Host= "
            + host + ", port= " + port);
         throw new UnknownHostException();
      } catch (IOException e) {
         _logger.error("Error creating input/output stream with Athena. Host= "
            + host + ", port= " + port);
         throw new IOException();
      }

      _logger.debug("Socket connection with Athena created");
   }

   /**
    * Closes the TCP connection
    */
   @Override
   public void close() {
      if (_disposed.get())
         return;

      if (_socket != null && !_socket.isClosed()) {
         try {
            _socket.close();
         } catch (IOException e) {
            _logger.error("Error in closing TCP socket");
         } finally {
            _disposed.set(true);
         }
      }
   }

   /**
    * Close the connection before garbage collection
    */
   @Override
   protected void finalize() throws Throwable {
      _logger.info("connection disposed");
      try {
         if (!_disposed.get())
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
         java.io.InputStream is = _socket.getInputStream();
         long start = System.currentTimeMillis();
         int msgSize = -1;
         byte[] msgSizeBuf = new byte[_receiveLengthSize];
         int msgSizeOffset = 0;
         byte[] result = null;
         int resultOffset = 0;

         while (System.currentTimeMillis() - start < _receiveTimeout) {
            // we need to read at least the header before we can do anything
            if (msgSizeOffset < _receiveLengthSize) {
               int count = is.read(msgSizeBuf,
                                   msgSizeOffset,
                                   _receiveLengthSize - msgSizeOffset);
               if (count < 0) {
                  _logger.error("No bytes read from athena");
                  break;
               } else {
                  msgSizeOffset += count;
               }
            }

            // we have the header - find out how big the body is
            if (msgSizeOffset == _receiveLengthSize && msgSize < 0) {
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
                  _logger.error("No bytes read from athena");
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
            _logger.error("Failed to receive message (" + resultOffset + " != "
               + msgSize + "). Closing socket.");
            close();
            return null;
         }

         return result;
      } catch (IOException e) {
         _logger.error("Failed to read from socket", e);
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
         _socket.getOutputStream().write(msg);
         return true;
      } catch (Exception e) {
         _logger.error("sendMessage", e);
         return false;
      }
   }

   @Override
   public boolean check() {
      try {
         _logger.trace("check enter");
         boolean res = AthenaHelper.sendToAthena(_athenaRequest, this, _conf);
         if (res) {
            Athena.AthenaResponse resp = AthenaHelper.receiveFromAthena(this);
            if (resp != null) {
               Athena.ProtocolResponse pResp = resp.getProtocolResponse();
               if (pResp != null) {
                  _logger.debug("check, got server version: "
                     + pResp.getServerVersion());
                  EthDispatcher.netVersion = pResp.getNetVersion();
                  return true;
               }
            }
         }

         return false;
      } catch (IOException e) {
         _logger.error("check", e);
         return false;
      } finally {
         _logger.trace("check exit");
      }
   }
}

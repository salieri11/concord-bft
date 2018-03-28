/**
 * This singleton class is used to maintain resources related to a single
 * TCP connection with Athena.
 * 
 * These resources are shared by all servlets.
 * This means that at present, only one servlet can talk to Athena at a
 * time.
 */
package connections;

import java.io.IOException;
import java.net.Socket;
import java.net.UnknownHostException;
import java.nio.ByteBuffer;
import java.util.concurrent.atomic.AtomicBoolean;
import org.apache.log4j.Logger;
import configurations.*;

public final class AthenaTCPConnection implements IAthenaConnection  {
   private Socket _socket;
   private AtomicBoolean _disposed;
   
   // this is naive implementation of timeout, the good one will be to have timeout defined per message type
   // since some calls may take more time in natural way, e.g. hard computations etc...
   private final int _receiveTimeout; //ms
   private final byte[] _checkMsg = new byte[1];
   private final int _receiveLengthSize; //bytes
   private IConfiguration _conf;
   private static Logger _logger = Logger.getLogger(AthenaTCPConnection.class);

   /**
    * Sets up a TCP connection with Athena and creates input and output streams
    * for this connection
    * 
    * @throws IOException
    */
   public AthenaTCPConnection(IConfiguration conf) throws IOException {
      _conf = conf;
      _receiveLengthSize = _conf.getIntegerValue("ReceiveHeaderSizeBytes");
      _receiveTimeout = _conf.getIntegerValue("ReceiveTimeoutMs");
      _disposed = new AtomicBoolean(false);
      String athenaHostName = _conf.getStringValue("AthenaHostName");
      int athenaPort = _conf.getIntegerValue("AthenaPort");

      // Create the TCP connection and input and output streams
      try {
    	  _socket = new Socket(athenaHostName, athenaPort);
    	  _socket.setTcpNoDelay(true);
    	  boolean res = check();
    	  _logger.trace(String.format("socket checked, result: %s", res));
      } catch (UnknownHostException e) {
         _logger.error("Error creating TCP connection with Athena");
         throw new UnknownHostException();
      } catch (IOException e) {
         _logger.error("Error creating input/output stream with Athena");
         throw new IOException();
      } finally {
 
      }
      _logger.debug("Socket connection with Athena created");
   }
   
   public boolean check( ) {
	   try {
		   _socket.getOutputStream().write(_checkMsg);
		   return true;
	   } catch (IOException e) {
		   _logger.error("check", e);
		   return false;
	   }
   }
   
   public void close() {
	   if(_disposed.get())
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
   
   @Override
   protected void finalize() throws Throwable {
	   _logger.info("connection disposed");
	   try {
		   if(!_disposed.get())
			   close();
	   } finally {
		   super.finalize();
	   }
   }

   @Override
   public byte[] receive() {
		try {
			java.io.InputStream is = _socket.getInputStream();
			long start = System.currentTimeMillis();
			ByteBuffer length = null;	
			byte[] res = null;
			int read = 0;
			boolean done = false;
			while(System.currentTimeMillis() - start < _receiveTimeout) {
				if(length == null && is.available() >= _receiveLengthSize) {
					length = ByteBuffer.wrap(new byte[_receiveLengthSize]);
					is.read(length.array(), 0, _receiveLengthSize);
				}
				
				if(length != null && res == null)
					res = new byte[length.getShort()];
				
				if (res != null) {
					int av = is.available();
					if(av > 0)
						read += is.read(res, read, av);
					if(read == length.getShort()) {
						done = true;
						break;
					}
				}
			}
			
			if(!done) {
				close();
				return null;
			}
	
			return res;
		} catch (IOException e) {
			_logger.error("readMessage", e);
			return null;
		}
	}

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
}

/**
 * This class contains helper functions for communicating with Athena
 */
package Servlets;

import com.vmware.athena.*;

import java.io.IOException;
import java.nio.ByteBuffer;
import java.nio.ByteOrder;
import org.apache.log4j.Logger;
import com.google.protobuf.InvalidProtocolBufferException;
import connections.IAthenaConnection;
import configurations.IConfiguration;

public class AthenaHelper {
   private static Logger _log = Logger.getLogger(AthenaHelper.class);

   /**
    * Converts an int into two bytes.
    * 
    * @param a
    *           Integer that needs to be converted
    * @return A byte array containing two bytes.
    */
   private static byte[] intToSizeBytes(int value, int size) {
      byte[] bytes = ByteBuffer.allocate(size).order(ByteOrder.LITTLE_ENDIAN)
               .putShort((short) value).array();
      return bytes;
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
   public static boolean sendToAthena(Athena.AthenaRequest request,
            IAthenaConnection conn, IConfiguration conf) throws IOException {
      // here specifically, request.toString() it time consuming,
      // so checking level enabled can gain performance
      if (_log.isTraceEnabled())
         _log.trace(String.format("Sending request to Athena : %s %s",
                  System.lineSeparator(), request));

      // Find size of request and pack size into two bytes.
      int requestSize = request.getSerializedSize();
      byte[] size = intToSizeBytes(requestSize,
               conf.getIntegerValue("ReceiveHeaderSizeBytes"));
      byte[] protobufRequest = request.toByteArray();
      ByteBuffer msg = ByteBuffer
               .allocate(size.length + protobufRequest.length);
      msg.put(size, 0, size.length);
      msg.put(protobufRequest, 0, protobufRequest.length);

      // Write requests over the output stream.
      boolean res = conn.send(msg.array());
      return res;
   }

   /**
    * Receives a Google Protocol Buffer response from Athena. Athena sends two
    * bytes signifying the size of the response before the actual response.
    * 
    * @param socketResponse
    *           InputStream object
    * @return Athena's response
    * @throws IOException
    **/
   public static Athena.AthenaResponse receiveFromAthena(
            IAthenaConnection conn) {
      try {
         byte[] data = conn.receive();
         if (data == null)
            return null;

         // Convert read bytes into a Protocol Buffer object.
         Athena.AthenaResponse athenaResponse;
         try {
            athenaResponse = Athena.AthenaResponse.parseFrom(data);
         } catch (InvalidProtocolBufferException e) {
            _log.error("Error in parsing Athena's response");
            throw new InvalidProtocolBufferException(e.getMessage());
         }

         return athenaResponse;
      } catch (Exception e) {
         _log.error("receiveFromAthena", e);
         return null;
      }
   }
}

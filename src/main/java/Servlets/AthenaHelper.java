package Servlets;

import com.vmware.athena.*;
import java.io.DataInputStream;
import java.io.DataOutputStream;
import java.io.IOException;
import java.io.PrintWriter;
import java.nio.ByteBuffer;
import java.util.ArrayList;
import java.util.List;
import javax.servlet.http.HttpServletResponse;
import org.apache.log4j.Logger;
import org.json.simple.JSONArray;
import org.json.simple.JSONObject;
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
      byte[] bytes = ByteBuffer
            .allocate(size)
            .putShort((short)value)
            .array();
      return bytes;
   }

   /**
    * Sends a Google Protocol Buffer request to Athena.
    * Athena expects two bytes signifying the size of the request
    * before the actual request.
    * @param socketRequest
    *           OutputStream object
    * @param request
    *           AthenaRequest object
    * @throws IOException
   */
   public static boolean sendToAthena(Athena.AthenaRequest request,
                                       IAthenaConnection conn,
                                       IConfiguration conf)
                         throws IOException {
      _log.trace(String.format("Sending request to Athena : %s %s",
            System.lineSeparator(), request));

      // Find size of request and pack size into two bytes.
      int requestSize = request.getSerializedSize();
      byte[] size = intToSizeBytes(requestSize, conf.getIntegerValue(
            "ReceiveHeaderSizeBytes"));
      byte[] protobufRequest = request.toByteArray();
      ByteBuffer msg = ByteBuffer.allocate(
            size.length + protobufRequest.length);
      msg.put(size, 0, size.length);
      msg.put(protobufRequest, size.length, protobufRequest.length);

      // Write requests over the output stream.
      boolean res = conn.send(msg.array());
      return res;
   }

   /**
    * Receives a Google Protocol Buffer response from Athena.
    * Athena sends two bytes signifying the size of the response 
    * before the actual response.
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

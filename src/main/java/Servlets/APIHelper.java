/**
 * Contains helper functions for API servlets.
 */
package Servlets;

import com.google.protobuf.ByteString;

public class APIHelper {

   /**
    * Converts a hex string into a binary string.
    * 
    * @param param
    *           A hex string
    * @return A ByteString object
    * @throws Exception
    */
   public static ByteString hexStringToBinary(String param) throws Exception {

      // Param should strictly be a hex string
      if (param == null || param.trim().length() < 1) {
         return null;
      }
      String curr = param.trim();

      if (curr.length() % 2 != 0) {
         return null;
      }

      int adjust = (curr.charAt(0) == '0' && curr.charAt(1) == 'x') ? 2 : 0;
      int resultSize = (curr.length() - adjust) / 2;

      byte[] resultBytes = new byte[resultSize];

      if (resultSize > 0) {
         for (int i = 0; i < resultSize; i++) {
            resultBytes[i] = (byte) ((hexVal(curr.charAt(i * 2 + adjust)) << 4)
                     | hexVal(curr.charAt(i * 2 + adjust + 1)));
         }
      }
      return ByteString.copyFrom(resultBytes);
   }

   /**
    * Converts a hex character into its corresponding numerical value
    * 
    * @param c
    * @return
    * @throws Exception
    */
   private static char hexVal(char c) throws Exception {
      if (c >= '0' && c <= '9') {
         return (char) (c - '0');
      } else if (c >= 'a' && c <= 'f') {
         return (char) (10 + c - 'a');
      } else if (c >= 'A' && c <= 'F') {
         return (char) (10 + c - 'A');
      } else {
         throw new Exception();
      }
   }

   /**
    * Converts a bytestring to a hex string.
    * 
    * @param binary
    *           Binary string
    * @return
    */
   public static String binaryStringToHex(ByteString binary) {
      byte[] resultBytes = binary.toByteArray();
      StringBuilder sb = new StringBuilder("0x");

      for (byte b : resultBytes) {
         sb.append((String.format("%02X ", b)));
      }
      String result = (sb.toString()).replace(" ", "");
      return result;
   }
}

/**
 * Contains helper functions for API servlets.
 */
package Servlets;

import org.bouncycastle.jcajce.provider.digest.Keccak;
import org.bouncycastle.util.encoders.Hex;

import com.google.protobuf.ByteString;
import org.json.simple.JSONObject;

import java.io.PrintWriter;
import java.io.StringWriter;

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

      if (curr.equals("0x")) {
         return ByteString.EMPTY;
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
         sb.append((String.format("%02x", b)));
      }
      String result = sb.toString();
      return result;
   }

   /**
    * Computes the Keccak-256 hash as per ethereum specifications
    *
    * @param hex
    * @return
    * @throws Exception
    */
   public static String getKeccak256Hash(String hex) throws Exception {
      String result = null;

      Keccak.Digest256 digest = new Keccak.Digest256();
      digest.update(hexStringToBinary(hex).toByteArray());
      byte[] res = digest.digest();
      result = "0x" + Hex.toHexString(res).toLowerCase();

      return result;
   }

   /**
    * Pads zeroes to the hex string to ensure a uniform length of 64 hex
    * characters
    *
    * @param p
    * @return
    */
   public static String padZeroes(String p) {
      int zeroes = 0;
      StringBuilder sb = new StringBuilder();
      if (p.startsWith("0x")) {
         p = p.substring(2);
      }
      if (p.length() < 64) {
         zeroes = 64 - p.length();
      }
      sb.append("0x");
      while (zeroes > 0) {
         sb.append("0");
         zeroes--;
      }
      sb.append(p);
      return sb.toString();
   }

   /**
    * An utility function to convert java exceptions stack trace into Strings.
    * 
    * @param e
    * @return string of exception stack trace
    */
   public static String exceptionToString(Exception e) {
      StringWriter sw = new StringWriter();
      PrintWriter pw = new PrintWriter(sw);
      e.printStackTrace(pw);
      return sw.toString(); // stack trace as a string
   }
   
   
   /**
    * Constructs the response in case of error.
    *
    * @param message
    *           Error message
    * @param id
    *           Request Id
    * @param jsonRpc
    *           RPC version
    * @return Error message string
    */
   @SuppressWarnings("unchecked")
   public static String errorMessage(String message, long id, String jsonRpc) {
      JSONObject responseJson = new JSONObject();
      responseJson.put("id", id);
      responseJson.put("jsonprc", jsonRpc);
      
      JSONObject error = new JSONObject();
      error.put("message", message);
      responseJson.put("error", error);
      
      return responseJson.toJSONString();
   }
   
   public static void fillErrorMessage(JSONObject object, String message, long
           id,
                                String jsonRpc) {
         object.put("id", id);
         object.put("jsonprc", jsonRpc);
      
         JSONObject error = new JSONObject();
         error.put("message", message);
         object.put("error", error);
         return;
   }
}

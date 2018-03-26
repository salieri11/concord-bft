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

public class AthenaHelper {
	private static Logger _log = Logger.getLogger(AthenaHelper.class);

	/**
	 * Converts an int into two bytes.
	 * 
	 * @param a
	 *            Integer that needs to be converted
	 * @return A byte array containing two bytes.
	 */
	private static byte[] intToSizeBytes(int a) {
		byte[] bytes = ByteBuffer.allocate(4).putInt(a).array();
		return bytes;
	}
	
	/**
	 * Sends a Google Protocol Buffer request to Athena. Athena expects two bytes
	 * signifying the size of the request before the actual request.
	 * 
	 * @param socketRequest
	 *            OutputStream object
	 * @param request
	 *            AthenaRequest object
	 * @throws IOException
	 */
	public static boolean sendToAthena(Athena.AthenaRequest request,
										IAthenaConnection conn) throws IOException {
		_log.trace(String.format("Sending request to Athena : %s %s", System.lineSeparator(), request));

		// Find size of request and pack size into two bytes.
		int requestSize = request.getSerializedSize();
		byte[] size = intToSizeBytes(requestSize);
		byte[] protobufRequest = request.toByteArray();

		// Write requests over the output stream.
		boolean res = conn.send(protobufRequest);
		return res;
	}

	/**
	 * Receives a Google Protocol Buffer response from Athena. Athena sends two
	 * bytes signifying the size of the response before the actual response.
	 * 
	 * @param socketResponse
	 *            InputStream object
	 * @return Athena's response
	 * @throws IOException
	 */
	public static Athena.AthenaResponse receiveFromAthena(IAthenaConnection conn) {
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

	public static void processResponse(HttpServletResponse resp, String data, int status) {
		try {
			// Set client response header
			resp.setHeader("Content-Transfer-Encoding", "UTF-8");
			resp.setContentType("application/json");
			resp.setStatus(status);
			if (data != null)
				resp.getWriter().write(data);
		} catch (Exception e) {
			_log.error("processResponse", e);
		}
	}
}

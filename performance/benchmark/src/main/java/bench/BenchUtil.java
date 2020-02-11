package bench;

import org.apache.logging.log4j.Logger;
import org.json.JSONObject;

import java.io.OutputStreamWriter;
import java.net.HttpURLConnection;
import java.net.URL;
import java.util.List;
import java.util.concurrent.TimeUnit;

import static org.apache.logging.log4j.LogManager.getLogger;

public class BenchUtil {
  private static final Logger logger = getLogger(BenchUtil.class);
  private static String configPath;

  public static JSONObject getJson(String server, String jsonString) {
    logger.info(server);
    HttpURLConnection urlConnection = null;
    JSONObject jsonObject = null;
    try {

      URL url = new URL(server);
      urlConnection = (HttpURLConnection) url.openConnection();
      urlConnection.setDoOutput(true);
      urlConnection.setRequestMethod("POST");
      urlConnection.setRequestProperty("Content-Type", "application/json");
      urlConnection.connect();
      OutputStreamWriter out = new OutputStreamWriter(urlConnection.getOutputStream());
      out.write(jsonString);
      out.close();
      int HttpResult = urlConnection.getResponseCode();
      String response = urlConnection.getResponseMessage();
      if (HttpResult == HttpURLConnection.HTTP_OK) {
        logger.info(response);
        jsonObject = new JSONObject(response);
      } else {
        logger.error("Error posting the request: " + urlConnection.getResponseMessage());
      }
    } catch (Exception e) {
      e.printStackTrace();
      jsonObject = new JSONObject();
      jsonObject.put("status", "false");
      jsonObject.put("message", e.getLocalizedMessage());
    } finally {
      if (urlConnection != null) urlConnection.disconnect();
    }
    return jsonObject;
  }

  /** Validate load distribution. */
  public static void validateLoadDistribution(List<Node> nodes) throws Exception {
    int total = nodes.stream().mapToInt(Node::getPercentage).sum();
    if (total != 100) {
      logger.warn("Percentage total mismatch! Expected: {}, Actual: {}", 100, total);
      throw new Exception("Load Percentage is not adding up to 100");
    }
  }

  public static String getConfigPath() {
    return configPath;
  }

  public static void setConfigPath(String path) {
    configPath = path;
  }

  public static void applyRateControl(long sleepTime, int idx, long start) {
    try {
      long diff = sleepTime * idx - (System.nanoTime() - start);

      if (diff > 0) {
        TimeUnit.NANOSECONDS.sleep(diff);
      }

    } catch (InterruptedException e) {
      e.printStackTrace();
    }
  }
}

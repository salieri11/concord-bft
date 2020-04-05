package bench;

import org.apache.logging.log4j.Logger;

import java.util.List;
import java.util.concurrent.TimeUnit;

import static org.apache.logging.log4j.LogManager.getLogger;

public class BenchUtil {
  private static final Logger logger = getLogger(BenchUtil.class);
  private static String configPath;

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

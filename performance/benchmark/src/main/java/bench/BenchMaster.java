package bench;

import java.io.File;
import java.io.FileInputStream;
import java.io.InputStream;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.yaml.snakeyaml.Yaml;
import org.yaml.snakeyaml.constructor.Constructor;

public class BenchMaster {

  private static final Logger logger = LogManager.getLogger(BenchMaster.class);

  @SuppressWarnings("unchecked")
  public static void main(String[] args) throws Exception {
    System.out.println("Starting DAppBench...");
    Yaml yaml = new Yaml(new Constructor(Benchmark.class));
    try {
      logger.info("Got config file: " + args[0]);
      BenchUtil.setConfigPath(args[0]);
      InputStream inputStream = new FileInputStream(new File(args[0]));
      Benchmark benchmark = yaml.load(inputStream);
      BenchController controller = new BenchController(benchmark);
      controller.startBenchMark();

    } catch (Exception e) {
      logger.error("Fatal error!", e);
      System.exit(-1);
    }
    logger.info("Finished benchmark!");
    System.exit(0);
  }
}

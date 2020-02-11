package bench;

import ballotapp.BallotAppManager;
import com.wavefront.sdk.common.WavefrontSender;
import com.wavefront.sdk.proxy.WavefrontProxyClient;
import dappbench.DAMLManager;
import org.apache.logging.log4j.Logger;

import java.io.File;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.text.DateFormat;
import java.text.SimpleDateFormat;
import java.util.Date;
import java.util.List;
import java.util.Optional;

import static org.apache.logging.log4j.LogManager.getLogger;

public class BenchController {
  private static final Logger logger = getLogger(BenchController.class);
  private SimpleConfig simpleConfig;
  private AdvancedConfig advancedConfig;
  List<Workload> workloads;
  private AdvancedConfig.Wavefront wavefront;
  Optional<WavefrontSender> optionalWavefrontSender;
  private String results;
  private String runResultsFolder;
  private List<Node> nodes;

  public BenchController(Benchmark benchmark) {
    try {
      simpleConfig = benchmark.getSimpleConfig();
      advancedConfig = benchmark.getAdvancedConfig();
      workloads = simpleConfig.getWorkloads();

      logger.info("workloads size: " + workloads.size());

      results = simpleConfig.getOutputDir();
      /* create the base results folder for benchmark */
      File directory = new File(results);
      if (!directory.exists()) {
        directory.mkdir();
      }

      /* Create the run specific results folder */
      DateFormat dateFormat = new SimpleDateFormat("yyyy-MM-dd HH:mm:ss");
      String timeStamp = dateFormat.format(new Date());
      runResultsFolder = "results_" + workloads.get(0).getDapp() + "_" + timeStamp;
      directory = new File(runResultsFolder);
      if (directory.exists()) {
        directory.delete();
        directory.mkdir();
      }

      nodes = benchmark.getSimpleConfig().getNodes();
      this.wavefront = advancedConfig.getWavefront();

      optionalWavefrontSender = createWavefrontSender();

    } catch (Exception e) {
      logger.error(e.getMessage());
      throw e;
    }
  }

  void startBenchMark() throws Exception {
    if (workloads.get(0).getDapp().equals("IOU")) {
      logger.info("Running DAML workloads");
      Workload workload = workloads.get(0);
      DAMLManager damlManager =
          new DAMLManager(workload, simpleConfig, advancedConfig, optionalWavefrontSender);
      damlManager.processDAMLTransactions(simpleConfig.getNodes(), simpleConfig.getPort());
      optionalWavefrontSender.ifPresent(this::close);

    } else if (workloads.get(0).getDapp().equals("Ballot")) {
      logger.info("Starting BallotApp");
      BallotAppManager ballotAppManager = new BallotAppManager(simpleConfig, advancedConfig, nodes);
      ballotAppManager.distributeTransaction();
      ballotAppManager.processTransaction();
    }

    // Move the log to run specific folder
    try {
      Path source = Paths.get(results);
      Path dest = Paths.get(runResultsFolder);
      Files.move(source, dest);
    } catch (Exception e) {
      logger.error(e.getMessage());
    }
  }

  /** Create optional WavefrontSender if configured. */
  private Optional<WavefrontSender> createWavefrontSender() {
    WavefrontProxyClient.Builder builder =
        wavefront.isEnabled()
            ? new WavefrontProxyClient.Builder(wavefront.getProxyHost())
                .metricsPort(wavefront.getMetricsPort())
            : null;
    return Optional.ofNullable(builder).map(WavefrontProxyClient.Builder::build);
  }

  /** Try to close WavefrontSender. */
  protected void close(WavefrontSender wavefrontSender) {
    try {
      wavefrontSender.close();
    } catch (IOException e) {
      logger.error("Unable to close WavefrontSender", e);
    }
  }
}

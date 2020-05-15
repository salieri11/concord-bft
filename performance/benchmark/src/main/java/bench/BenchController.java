package bench;

import static org.apache.logging.log4j.LogManager.getLogger;

import ballotapp.BallotAppManager;
import com.wavefront.sdk.common.WavefrontSender;
import com.wavefront.sdk.proxy.WavefrontProxyClient;
import dappbench.DAMLManager;
import dappbench.WorkloadManager;
import pee.PeeManager;
import tee.TeeManager;
import java.io.IOException;
import java.util.List;
import java.util.Optional;
import org.apache.logging.log4j.Logger;

public class BenchController {

  private static final Logger logger = getLogger(BenchController.class);
  private SimpleConfig simpleConfig;
  private AdvancedConfig advancedConfig;
  List<Workload> workloads;
  private AdvancedConfig.Wavefront wavefront;
  Optional<WavefrontSender> optionalWavefrontSender;
  private List<Node> nodes;

  public BenchController(Benchmark benchmark) {
    try {
      simpleConfig = benchmark.getSimpleConfig();
      advancedConfig = benchmark.getAdvancedConfig();
      workloads = simpleConfig.getWorkloads();

      nodes = benchmark.getSimpleConfig().getNodes();
      this.wavefront = advancedConfig.getWavefront();

      optionalWavefrontSender = createWavefrontSender();

    } catch (Exception e) {
      logger.error(e.getMessage());
      throw e;
    }
  }

  void startBenchMark() throws Exception {
    String workloadName = workloads.get(0).getDapp();
    logger.info("Running {} workload", workloadName);

    if (workloadName.equals("IOU")) {
      Workload workload = workloads.get(0);
      WorkloadManager damlManager = new DAMLManager(workload, simpleConfig, advancedConfig, optionalWavefrontSender);
      damlManager.executeWorkload();
      tearDown(damlManager);
    } else if (workloadName.equals("TEE")) {
      Workload workload = workloads.get(0);
      WorkloadManager teeManager = new TeeManager(workload, simpleConfig, advancedConfig, optionalWavefrontSender);
      teeManager.executeWorkload();
      tearDown(teeManager);
    } else if (workloadName.equals("PEE")) {
        Workload workload = workloads.get(0);
        WorkloadManager peeManager = new PeeManager(workload, simpleConfig, advancedConfig, optionalWavefrontSender);
        peeManager.executeWorkload();
        tearDown(peeManager);
    } else if (workloadName.equals("Ballot")) {
      BallotAppManager ballotAppManager = new BallotAppManager(simpleConfig, advancedConfig, nodes);
      ballotAppManager.distributeTransaction();
      ballotAppManager.processTransaction();
    }
  }

  /**
   * Post execution cleanup.
   */
  private void tearDown(WorkloadManager workloadManager) throws IOException {
    optionalWavefrontSender.ifPresent(this::close);
    workloadManager.tearDown();
  }

  /**
   * Create optional WavefrontSender if configured.
   */
  private Optional<WavefrontSender> createWavefrontSender() {
    WavefrontProxyClient.Builder builder =
        wavefront.isEnabled()
            ? new WavefrontProxyClient.Builder(wavefront.getProxyHost())
            .metricsPort(wavefront.getMetricsPort())
            : null;
    return Optional.ofNullable(builder).map(WavefrontProxyClient.Builder::build);
  }

  /**
   * Try to close WavefrontSender.
   */
  protected void close(WavefrontSender wavefrontSender) {
    try {
      wavefrontSender.close();
    } catch (IOException e) {
      logger.error("Unable to close WavefrontSender", e);
    }
  }
}

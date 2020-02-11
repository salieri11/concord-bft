package dappbench;

import bench.*;
import com.daml.ledger.javaapi.data.Command;
import com.digitalasset.quickstart.model.iou.Iou;
import com.wavefront.sdk.common.WavefrontSender;
import me.tongfei.progressbar.ProgressBar;
import org.apache.logging.log4j.Logger;

import java.io.IOException;
import java.math.BigDecimal;
import java.time.Duration;
import java.time.Instant;
import java.util.*;
import java.util.concurrent.CountDownLatch;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.atomic.LongAdder;

import static java.lang.Integer.parseInt;
import static java.lang.Math.max;
import static java.lang.Math.round;
import static java.lang.System.currentTimeMillis;
import static java.lang.System.nanoTime;
import static java.time.Instant.now;
import static java.util.Collections.*;
import static java.util.concurrent.Executors.newFixedThreadPool;
import static java.util.concurrent.TimeUnit.SECONDS;
import static java.util.stream.Collectors.toMap;
import static org.apache.logging.log4j.LogManager.getLogger;

/**
 * This acts as a client side load balancer which distributes transactions between multiple DAML
 * ledger nodes.
 */
public class DAMLManager {
  private static final Logger logger = getLogger(DAMLManager.class);

  private final String party;
  private final int numOfTransactions;
  private final int rateControl;
  private final boolean logging;
  private final int concurrency;
  private SimpleConfig simpleConfig;
  private final Optional<WavefrontSender> optionalWavefrontSender;
  private AdvancedConfig.Wavefront wavefront;
  private Map<String, List<String>> stats;
  private String testName;
  private Data data;

  public DAMLManager(
      Workload workload,
      SimpleConfig simpleconfig,
      AdvancedConfig advancedConfig,
      Optional<WavefrontSender> optionalWavefrontSender) {
    List<String> params = workload.getParams();
    this.party = params.get(0);
    this.numOfTransactions = parseInt(params.get(1));
    this.rateControl = workload.getRateControl();
    this.logging = workload.getLogging();
    this.concurrency = simpleconfig.getNumberThreads();
    this.optionalWavefrontSender = optionalWavefrontSender;
    this.wavefront = advancedConfig.getWavefront();
    testName = workload.getDapp();
    this.simpleConfig = simpleconfig;

    logger.info("Total transactions: {}", numOfTransactions);
    logger.info("Concurrency: {}", concurrency);
    logger.info("Rate control: {} tps", rateControl);

    // Will be used in reporting
    data = new Data();
    data.setAppSummaryTableHeader(
        Arrays.asList(
            "WorkloadName",
            "Concurrency",
            "Rate Control",
            "TestDuration",
            "Throughput",
            "Average gRPC response time",
            "Total transactions",
            "Successful transactions"));
    data.addBasicInformation("DLT", simpleconfig.getBlockchain());
    data.setConfigFilePath("../" + BenchUtil.getConfigPath());
  }

  /** Creates IOU commands and executes them on given nodes. */
  public void processDAMLTransactions(List<Node> nodes, int ledgerPort) throws Exception {
    Map<DamlClient, Long> clientToTx = initClients(nodes, ledgerPort);
    logger.info("Expected transactions: {}", clientToTx);

    Collection<DamlClient> clients = clientToTx.keySet();
    logger.info("Ledger size before test: {}", getLedgerSize(clients));

    List<DamlClient> txClients = assignClients(clientToTx);

    Random random = new Random();
    long startTimeNanos = nanoTime();

    logger.info("Starting transaction ...");

    ExecutorService executorService = newFixedThreadPool(concurrency);
    CountDownLatch countDownLatch = new CountDownLatch(numOfTransactions);
    ProgressBar progressBar = new ProgressBar("Transaction Progress", numOfTransactions);

    LongAdder totalResponseTimeMillis = new LongAdder();

    for (int i = 0; i < txClients.size(); i++) {
      int iouAmount = random.nextInt(10_000) + 1;
      Iou iou = new Iou("Alice", "Alice", "AliceCoin", new BigDecimal(iouAmount), emptyList());
      if (logging) {
        logger.info("{}", iou);
      }
      Command command = iou.create();
      DamlClient client = txClients.get(i);

      executorService.execute(
          () -> {
            Instant start = now();
            try {
              client.submitIou(command, party);
            } finally {
              Duration responseTime = Duration.between(start, now());
              totalResponseTimeMillis.add(responseTime.toMillis());
              optionalWavefrontSender.ifPresent(
                  wavefrontSender ->
                      sendMetric(wavefrontSender, responseTime, client.getLedgerHost()));
              countDownLatch.countDown();
              progressBar.step();
            }
          });

      if (rateControl != 0) {
        // Average gap between transactions.
        long timeToSleep = SECONDS.toNanos(1) / rateControl;
        BenchUtil.applyRateControl(timeToSleep, i, startTimeNanos);
      }
    }

    executorService.shutdown();
    await(countDownLatch);
    progressBar.close();

    Duration testTime = Duration.ofNanos(nanoTime() - startTimeNanos);
    summarize(testTime, totalResponseTimeMillis.longValue(), clients);
    data.addAppSummaryTableData(getStats());

    Reporting report = new Reporting(data);
    report.process(simpleConfig.getOutputDir());
  }

  /** Await on the countdown latch. */
  private void await(CountDownLatch countDownLatch) {
    try {
      countDownLatch.await();
    } catch (InterruptedException e) {
      logger.error("Thread interrupted", e);
    }
  }

  /** Create client for each node and determine transaction count for it. */
  private Map<DamlClient, Long> initClients(List<Node> nodes, int ledgerPort) throws Exception {
    BenchUtil.validateLoadDistribution(nodes);

    Map<DamlClient, Long> clientToTx = new HashMap<>();

    for (Node node : nodes) {
      logger.info(node.getIp());
      if (node.getPercentage() == 0) {
        continue;
      }

      DamlClient client = new DamlClient(node.getIp(), ledgerPort);
      client.init();

      long txCount = round(numOfTransactions * ((double) node.getPercentage() / 100));
      clientToTx.put(client, txCount);
    }

    adjustResidue(clientToTx);

    return clientToTx;
  }

  /**
   * Make sure that sum of transaction count of each node is equal to the total transaction count.
   */
  private void adjustResidue(Map<DamlClient, Long> clientToTx) {
    int numOfAssignedTx = clientToTx.values().stream().mapToInt(Long::intValue).sum();
    if (numOfTransactions != numOfAssignedTx) {
      logger.warn(
          "Discrepancy between expected [{}] and assigned [{}] tx",
          numOfTransactions,
          numOfAssignedTx);
    }
  }

  private List<DamlClient> assignClients(Map<DamlClient, Long> clientToTx) {
    List<DamlClient> clients = new ArrayList<>(numOfTransactions);

    clientToTx.forEach(
        (client, txCount) -> {
          while (txCount > 0) {
            clients.add(client);
            txCount--;
          }
        });

    shuffle(clients);
    return clients;
  }

  /** Get submitted transaction count for each node. */
  private Map<String, Integer> getSubmissionCount(Collection<DamlClient> clients) {
    return clients.stream().collect(toMap(DamlClient::getLedgerHost, DamlClient::getTxCount));
  }

  /** Send transaction response time to Wavefront. */
  private void sendMetric(
      WavefrontSender wavefrontSender, Duration responseTime, String ledgerHost) {
    try {
      wavefrontSender.sendMetric(
          "submitAndWait.response.ms",
          responseTime.toMillis(),
          currentTimeMillis(),
          wavefront.getSource(),
          singletonMap("ledgerHost", ledgerHost));
    } catch (IOException e) {
      logger.warn("Error sending metrics to Wavefront", e);
    }
  }

  /** Get ledger size for each node. */
  private Map<String, Long> getLedgerSize(Collection<DamlClient> clients) {
    return clients.stream()
        .collect(toMap(DamlClient::getLedgerHost, DamlClient::getCurrentLedgerOffset));
  }

  /** Summarize the result. */
  private void summarize(
      Duration testTime, long totalResponseTimeMillis, Collection<DamlClient> clients) {

    logger.info("Total duration of test: {}", testTime);
    logger.info("Throughput: {} tps", numOfTransactions / max(testTime.getSeconds(), 1));
    logger.info("Average gRPC response time: {} ms", totalResponseTimeMillis / numOfTransactions);

    logger.info("Successful transactions: {}", getSubmissionCount(clients));
    logger.info("Ledger size after test: {}", getLedgerSize(clients));

    stats = new HashMap<>();
    stats.put(
        "tableRow",
        Arrays.asList(
            testName,
            String.valueOf(concurrency),
            String.valueOf(rateControl) + " tps",
            String.valueOf(testTime),
            String.valueOf(numOfTransactions / max(testTime.getSeconds(), 1)) + " tps",
            String.valueOf(totalResponseTimeMillis / numOfTransactions) + " ms",
            String.valueOf(numOfTransactions),
            String.valueOf(getSubmissionCount(clients))));
  }

  Map<String, List<String>> getStats() {
    return this.stats;
  }
}

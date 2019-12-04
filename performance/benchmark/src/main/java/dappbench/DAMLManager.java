package dappbench;

import com.daml.ledger.javaapi.data.Command;
import com.digitalasset.quickstart.model.iou.Iou;
import com.vmware.blockchain.performance.Utils;
import com.wavefront.sdk.common.WavefrontSender;
import com.wavefront.sdk.proxy.WavefrontProxyClient;
import me.tongfei.progressbar.ProgressBar;
import org.apache.logging.log4j.Logger;

import java.io.IOException;
import java.math.BigDecimal;
import java.time.Duration;
import java.time.Instant;
import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.Random;
import java.util.concurrent.CountDownLatch;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.atomic.LongAdder;

import static java.lang.Integer.parseInt;
import static java.lang.Math.max;
import static java.lang.Math.round;
import static java.lang.System.*;
import static java.time.Instant.now;
import static java.util.Collections.*;
import static java.util.concurrent.Executors.newFixedThreadPool;
import static java.util.concurrent.TimeUnit.SECONDS;
import static java.util.stream.Collectors.toMap;
import static org.apache.logging.log4j.LogManager.getLogger;

/**
 * This acts as a client side load balancer which distributes transactions
 * between multiple DAML ledger nodes.
 */
public class DAMLManager {
    private final static Logger logger = getLogger(DAMLManager.class);

    private final String party;
    private final int numOfTransactions;
    private final int rateControl;
    private final boolean logging;
    private final int concurrency;
    private final AdvancedConfig.Wavefront wavefront;

    public DAMLManager(Workload workload, AdvancedConfig advancedConfig) {
        List<String> params = workload.getParams();
        this.party = params.get(0);
        this.numOfTransactions = parseInt(params.get(1));
        this.rateControl = workload.getRateControl();
        this.logging = workload.getLogging();
        this.concurrency = advancedConfig.getNumberThreads();
        this.wavefront = advancedConfig.getWavefront();

        logger.info("Total transactions: {}", numOfTransactions);
        logger.info("Concurrency: {}", concurrency);
        logger.info("Rate control: {} tps", rateControl);
    }

    /**
     * Creates IOU commands and executes them on given nodes.
     */
    public void processDAMLTransactions(List<Node> nodes, int ledgerPort) {
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
        Optional<WavefrontSender> optionalWavefrontSender = createWavefrontSender();

        LongAdder totalResponseTimeMillis = new LongAdder();

        for (int i = 0; i < txClients.size(); i++) {
            int iouAmount = random.nextInt(10_000) + 1;
            Iou iou = new Iou("Alice", "Alice", "AliceCoin", new BigDecimal(iouAmount), emptyList());
            if (logging) {
                logger.info("{}", iou);
            }
            Command command = iou.create();
            DamlClient client = txClients.get(i);

            executorService.execute(() -> {
                Instant start = now();
                try {
                    client.submitIou(command, party);
                } finally {
                    Duration responseTime = Duration.between(start, now());
                    totalResponseTimeMillis.add(responseTime.toMillis());
                    optionalWavefrontSender.ifPresent(wavefrontSender -> sendMetric(wavefrontSender, responseTime, client.getLedgerHost()));
                    countDownLatch.countDown();
                    progressBar.step();
                }
            });

            if (rateControl != 0) {
                // Average gap between transactions.
                long timeToSleep = SECONDS.toNanos(1) / rateControl;
                Utils.applyRateControl(timeToSleep, i, startTimeNanos);
            }
        }

        executorService.shutdown();
        await(countDownLatch);
        progressBar.close();
        optionalWavefrontSender.ifPresent(this::close);

        Duration testTime = Duration.ofNanos(nanoTime() - startTimeNanos);
        summarize(testTime, totalResponseTimeMillis.longValue(), clients);
    }

    /**
     * Create optional WavefrontSender if configured.
     */
    private Optional<WavefrontSender> createWavefrontSender() {
        WavefrontProxyClient.Builder builder = wavefront.isEnabled() ? new WavefrontProxyClient.Builder(wavefront.getProxyHost()).metricsPort(wavefront.getMetricsPort()) : null;
        return Optional.ofNullable(builder).map(WavefrontProxyClient.Builder::build);
    }

    /**
     * Await on the countdown latch.
     */
    private void await(CountDownLatch countDownLatch) {
        try {
            countDownLatch.await();
        } catch (InterruptedException e) {
            logger.error("Thread interrupted", e);
        }
    }

    /**
     * Try to close WavefrontSender.
     */
    private void close(WavefrontSender wavefrontSender) {
        try {
            wavefrontSender.close();
        } catch (IOException e) {
            logger.error("Unable to close WavefrontSender", e);
        }
    }

    /**
     * Send transaction response time to Wavefront.
     */
    private void sendMetric(WavefrontSender wavefrontSender, Duration responseTime, String ledgerHost) {
        try {
            wavefrontSender.sendMetric("submitAndWait.response.ms", responseTime.toMillis(), currentTimeMillis(), wavefront.getSource(), singletonMap("ledgerHost", ledgerHost));
        } catch (IOException e) {
            logger.warn("Error sending metrics to Wavefront", e);
        }
    }

    /**
     * Create client for each node and determine transaction count for it.
     */
    private Map<DamlClient, Long> initClients(List<Node> nodes, int ledgerPort) {
        validate(nodes);

        Map<DamlClient, Long> clientToTx = new HashMap<>();

        for (Node node : nodes) {
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
     * Make sure that sum of transaction count of each node is equal to the total
     * transaction count.
     */
    private void adjustResidue(Map<DamlClient, Long> clientToTx) {
        int numOfAssignedTx = clientToTx.values().stream().mapToInt(Long::intValue).sum();
        if (numOfTransactions != numOfAssignedTx) {
            logger.warn("Discrepancy between expected [{}] and assigned [{}] tx", numOfTransactions, numOfAssignedTx);
        }
    }

    private List<DamlClient> assignClients(Map<DamlClient, Long> clientToTx) {
        List<DamlClient> clients = new ArrayList<>(numOfTransactions);

        clientToTx.forEach((client, txCount) -> {
            while (txCount > 0) {
                clients.add(client);
                txCount--;
            }
        });

        shuffle(clients);
        return clients;
    }

    /**
     * Validate load distribution.
     */
    private void validate(List<Node> nodes) {
        int total = nodes.stream().mapToInt(Node::getPercentage).sum();
        if (total != 100) {
            logger.warn("Percentage total mismatch! Expected: {}, Actual: {}", 100, total);
            exit(0);
        }
    }

    /**
     * Get submitted transaction count for each node.
     */
    private Map<String, Integer> getSubmissionCount(Collection<DamlClient> clients) {
        return clients.stream().collect(toMap(DamlClient::getLedgerHost, DamlClient::getTxCount));
    }

    /**
     * Get ledger size for each node.
     */
    private Map<String, Long> getLedgerSize(Collection<DamlClient> clients) {
        return clients.stream().collect(toMap(DamlClient::getLedgerHost, DamlClient::getCurrentLedgerOffset));
    }

    /**
     * Summarize the result.
     */
    private void summarize(Duration testTime, long totalResponseTimeMillis, Collection<DamlClient> clients) {
        logger.info("Total duration of test: {}", testTime);
        logger.info("Throughput: {} tps", numOfTransactions / max(testTime.getSeconds(), 1));
        logger.info("Average gRPC response time: {} ms", totalResponseTimeMillis / numOfTransactions);

        logger.info("Successful transactions: {}", getSubmissionCount(clients));
        logger.info("Ledger size after test: {}", getLedgerSize(clients));
    }

}

package dappbench;

import bench.*;
import bench.AdvancedConfig.Wavefront;
import com.wavefront.sdk.common.WavefrontSender;
import io.grpc.Status;
import me.tongfei.progressbar.ProgressBar;
import org.apache.logging.log4j.Logger;
import org.json.JSONObject;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.text.SimpleDateFormat;
import java.time.Duration;
import java.time.Instant;
import java.util.*;
import java.util.concurrent.CountDownLatch;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.atomic.LongAdder;

import static java.lang.Math.max;
import static java.lang.Math.round;
import static java.lang.String.format;
import static java.lang.String.valueOf;
import static java.lang.System.currentTimeMillis;
import static java.lang.System.nanoTime;
import static java.time.Instant.now;
import static java.util.Collections.shuffle;
import static java.util.Collections.singletonMap;
import static java.util.concurrent.Executors.newFixedThreadPool;
import static java.util.concurrent.TimeUnit.SECONDS;
import static java.util.stream.Collectors.toMap;
import static org.apache.logging.log4j.LogManager.getLogger;

/**
 * Common workload manager that schedules the workload execution and reports the result.
 */
public abstract class WorkloadManager {
    private static final Logger logger = getLogger(WorkloadManager.class);

    private final List<Node> nodes;

    private final String testName;
    private int numOfRequests;
    private final int rateControl;
    private final int concurrency;

    private final Optional<WavefrontSender> optionalWavefrontSender;
    private final Wavefront wavefront;

    private final Map<String, List<String>> stats;
    private WorkloadStats workloadStats;
    private final Map<String, Object> jsonStats;
    private final Data data;
    private final String outputDir;

    public WorkloadManager(Workload workload, SimpleConfig simpleconfig, AdvancedConfig advancedConfig, Optional<WavefrontSender> optionalWavefrontSender) {
        this.testName = workload.getDapp();
        this.concurrency = simpleconfig.getNumberThreads();
        this.rateControl = workload.getRateControl();

        logger.info("Concurrency: {}", concurrency);
        logger.info("Rate control: {} rps", rateControl);

        this.nodes = simpleconfig.getNodes();

        this.optionalWavefrontSender = optionalWavefrontSender;
        this.wavefront = advancedConfig.getWavefront();

        this.outputDir = simpleconfig.getOutputDir();

        // Will be used in reporting
        stats = new HashMap<>();
        jsonStats = new HashMap<>();
        data = new Data();
        data.setAppSummaryTableHeader(
                Arrays.asList(
                        "Workload",
                        "Concurrency",
                        "Rate Control",
                        "Test Duration",
                        "Throughput",
                        "Average Response Time",
                        "Total Requests",
                        "Response Status"));
        data.addBasicInformation("DLT", simpleconfig.getBlockchain());
        data.setConfigFilePath("../" + BenchUtil.getConfigPath());
    }

    /**
     * Get blockchain nodes.
     */
    public List<Node> getNodes() {
        return nodes;
    }

    /**
     * Get total number of requests to be executed.
     */
    protected abstract int getRequestCount();

    /**
     * Get type of operation.
     */
    protected abstract String getOperationType();

    /**
     * Create client specific to the type of workload.
     */
    protected abstract WorkloadClient createClient(String host, int port);

    /**
     * Execute workloads on the given nodes.
     */
    public void executeWorkload() throws Exception {
        numOfRequests = getRequestCount();
        logger.info("Total requests: {}", numOfRequests);

        Map<WorkloadClient, Long> clientToRequestCount = initClients();
        logger.info("Request distribution: {}", clientToRequestCount);

        Collection<WorkloadClient> clients = clientToRequestCount.keySet();

        List<WorkloadClient> requestClients = assignClients(clientToRequestCount);

        long startTimeNanos = nanoTime();

        logger.info("Executing {} workload ..", testName);

        ExecutorService executorService = newFixedThreadPool(concurrency);
        CountDownLatch countDownLatch = new CountDownLatch(numOfRequests);
        ProgressBar progressBar = new ProgressBar("Workload Progress", numOfRequests);

        LongAdder totalResponseTimeMillis = new LongAdder();

        for (int i = 0; i < requestClients.size(); i++) {
            WorkloadClient client = requestClients.get(i);

            executorService.execute(
                    () -> {
                        Instant start = now();
                        try {
                            client.execute();
                        } finally {
                            Duration responseTime = Duration.between(start, now());
                            totalResponseTimeMillis.add(responseTime.toMillis());
                            optionalWavefrontSender.ifPresent(wavefrontSender -> sendMetric(wavefrontSender, responseTime, client.getHost()));
                            countDownLatch.countDown();
                            progressBar.step();
                        }
                    });

            if (rateControl != 0) {
                // Average gap between requests.
                long timeToSleep = SECONDS.toNanos(1) / rateControl;
                BenchUtil.applyRateControl(timeToSleep, i, startTimeNanos);
            }
        }

        executorService.shutdown();
        await(countDownLatch);
        progressBar.close();

        Duration testTime = Duration.ofNanos(nanoTime() - startTimeNanos);
        workloadStats = createStats(testTime, totalResponseTimeMillis.longValue(), clients);
        summarize(workloadStats);
        data.addAppSummaryTableData(getStats());

        Reporting report = new Reporting(data);
        report.process(outputDir);
    }

    /**
     * Post execution steps.
     */
    public void tearDown() throws IOException {
        jsonStats.put(testName, workloadStats);
        String timestamp = new SimpleDateFormat("yyyyMMddHHmm").format(new Date());
        Path file = Paths.get(outputDir, format("report-%s.json", timestamp));
        Files.write(file, new JSONObject(jsonStats).toString(4).getBytes());
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
     * Create client for each node and determine no. of requests it is supposed to execute.
     */
    private Map<WorkloadClient, Long> initClients() throws Exception {
        BenchUtil.validateLoadDistribution(nodes);

        Map<WorkloadClient, Long> clientToRequestCount = new HashMap<>();

        for (Node node : nodes) {
            if (node.getPercentage() == 0) {
                continue;
            }
            logger.info("{} - Load share: {}%", node, node.getPercentage());
            WorkloadClient client = createClient(node.getIp(), node.getPort());

            long requestCountForNode = round(numOfRequests * ((double) node.getPercentage() / 100));
            clientToRequestCount.put(client, requestCountForNode);
        }

        adjustResidue(clientToRequestCount);

        return clientToRequestCount;
    }

    /**
     * Make sure that sum of request count of each node is equal to the total request count.
     */
    private void adjustResidue(Map<WorkloadClient, Long> clientToRequestCount) {
        int numOfAssignedRequests = clientToRequestCount.values().stream().mapToInt(Long::intValue).sum();
        if (numOfRequests != numOfAssignedRequests) {
            logger.warn("Discrepancy between expected [{}] and assigned [{}] request count", numOfRequests, numOfAssignedRequests);
        }
    }

    /**
     * Assign workloads to clients.
     */
    private List<WorkloadClient> assignClients(Map<WorkloadClient, Long> clientToRequestCount) {
        List<WorkloadClient> clients = new ArrayList<>(numOfRequests);

        clientToRequestCount.forEach((client, requestCountForNode) -> {
            while (requestCountForNode > 0) {
                clients.add(client);
                requestCountForNode--;
            }
        });

        shuffle(clients);
        return clients;
    }

    /**
     * No. of requests group by status, node
     */
    private Map<String, Map<Status.Code, LongAdder>> getStatusCount(Collection<WorkloadClient> clients) {
        return clients.stream().collect(toMap(WorkloadClient::toString, WorkloadClient::getStatusCount));
    }

    /**
     * Send response time to Wavefront.
     */
    private void sendMetric(WavefrontSender wavefrontSender, Duration responseTime, String host) {
        try {
            wavefrontSender.sendMetric(wavefront.getMetricName(), responseTime.toMillis(), currentTimeMillis(), wavefront.getSource(), singletonMap("host", host));
        } catch (IOException e) {
            logger.warn("Error sending metrics to Wavefront", e);
        }
    }

    /**
     * Summarize the result in Console and save as HTML.
     */
    private void summarize(WorkloadStats s) {
        logger.info("Total duration of test: {}", s.getTestDuration());
        logger.info("Throughput: {} rps", s.getRps());
        logger.info("Average gRPC response time: {} ms", s.getAverageResponseTimeMillis());
        logger.info("Response status: {}", s.getResponseStatus());

        stats.put(
                "tableRow",
                Arrays.asList(
                        s.getOperation(),
                        valueOf(s.getConcurrency()),
                        s.getRateControl() + " rps",
                        valueOf(s.getTestDuration()),
                        s.getRps() + " rps",
                        s.getAverageResponseTimeMillis() + " ms",
                        valueOf(s.getTotalRequests()),
                        valueOf(s.getResponseStatus())));
    }

    /**
     * Create workload stats.
     */
    private WorkloadStats createStats(Duration testTime, long totalResponseTimeMillis, Collection<WorkloadClient> clients) {
        WorkloadStats s = new WorkloadStats();
        s.setOperation(getOperationType());
        s.setConcurrency(concurrency);
        s.setRateControl(rateControl);
        s.setTestDuration(testTime);
        s.setTotalRequests(numOfRequests);
        s.setRps(numOfRequests / max(testTime.getSeconds(), 1));
        s.setAverageResponseTimeMillis(totalResponseTimeMillis / numOfRequests);
        s.setResponseStatus(getStatusCount(clients));
        return s;
    }

    /**
     * HTML stats
     */
    public Map<String, List<String>> getStats() {
        return this.stats;
    }

    /**
     * JSON stats
     */
    public Map<String, Object> getJsonStats() {
        return jsonStats;
    }
}

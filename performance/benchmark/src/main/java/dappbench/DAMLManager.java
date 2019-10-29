package dappbench;

import static java.lang.Math.round;
import static java.lang.System.exit;
import static java.lang.System.nanoTime;
import static java.time.Duration.ofNanos;
import static java.util.Collections.emptyList;
import static java.util.Collections.shuffle;
import static java.util.concurrent.TimeUnit.SECONDS;
import static java.util.stream.Collectors.summingInt;
import static java.util.stream.Collectors.toMap;
import static org.apache.logging.log4j.LogManager.getLogger;

import java.math.BigDecimal;
import java.time.Duration;
import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Random;

import org.apache.logging.log4j.Logger;

import com.daml.ledger.javaapi.data.Command;
import com.digitalasset.quickstart.model.iou.Iou;
import com.vmware.blockchain.performance.Utils;

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

    public DAMLManager(Workload workload) {
        List<String> params = workload.getParams();
        this.party = params.get(0);
        this.numOfTransactions = Integer.parseInt(params.get(1));
        this.rateControl = workload.getRateControl();
        this.logging = workload.getLogging();

        logger.info("Total transactions: {}", numOfTransactions);
        logger.info("Rate control: {} tps", rateControl);
    }

    /**
     * Creates IOU commands and executes them on given nodes.
     */
    public void processDAMLTransactions(List<Node> nodes, int ledgerPort) {
        Map<DamlClient, Long> clientToTx = initClients(nodes, ledgerPort);
        logger.info("Expected tx distribution: {}", clientToTx);
        List<DamlClient> clients = assignClients(clientToTx);

        Random random = new Random();
        long startTime = nanoTime();

        logger.info("Starting transaction ...");

        for (int i = 0; i < clients.size(); i++) {
            int iouAmount = random.nextInt(10_000) + 1;
            Iou iou = new Iou("Alice", "Alice", "AliceCoin", new BigDecimal(iouAmount), emptyList());
            if (logging) {
                logger.info("{}", iou);
            }
            Command command = iou.create();
            DamlClient client = clients.get(i);
            client.submitIou(command, party);

            if (rateControl != 0) {
                // Average gap between transactions.
                long timeToSleep = SECONDS.toNanos(1) / rateControl;
                Utils.applyRateControl(timeToSleep, i, startTime);
            }
        }

        long endTime = nanoTime();
        summarize(ofNanos(endTime - startTime), clientToTx.keySet());
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
        int total = nodes.stream().collect(summingInt(Node::getPercentage));
        if (total != 100) {
            logger.warn("Percentage total mismatch! Expected: {}, Actual: {}", 100, total);
            exit(0);
        }
    }

    /**
     * Summarize the result.
     */
    private void summarize(Duration totalTime, Collection<DamlClient> clients) {
        clients.forEach(client -> {
            logger.info("No. of active contracts: {}", client.getActiveContractCount(party));
        });
        logger.info("Total time taken: {} s", totalTime.getSeconds());
        logger.info("Average latency: {} ms", totalTime.toMillis() / numOfTransactions);
        logger.info("Throughput: {} tps", numOfTransactions / totalTime.getSeconds());
        logger.info("Distribution: {}", clients.stream().collect(toMap(DamlClient::getLedgerHost, DamlClient::getTxCount)));
    }

}

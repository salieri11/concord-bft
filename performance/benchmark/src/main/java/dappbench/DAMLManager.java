package dappbench;

import static java.lang.Math.round;
import static java.lang.System.exit;
import static java.util.Collections.emptyList;
import static java.util.Collections.shuffle;
import static java.util.stream.Collectors.summingInt;
import static java.util.stream.Collectors.toMap;
import static org.apache.logging.log4j.LogManager.getLogger;

import java.math.BigDecimal;
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

    private int ledgerPort;
    private String party;
    private int rateControl;
    private int numOfTransactions;

    public DAMLManager(Workload workload) {
        List<String> params = workload.getParams();
        this.ledgerPort = Integer.parseInt(params.get(0));
        this.party = params.get(1);
        this.numOfTransactions = Integer.parseInt(params.get(2));
        this.rateControl = workload.getRateControl();
        logger.info("Total number of Transactions {}", numOfTransactions);
        logger.info("Rate control value: {}", rateControl);
    }

    /**
     * Creates IOU commands and executes them on given nodes.
     */
    public void processDAMLTransactions(List<Node> nodes) {
        Map<DamlClient, Long> clientToTx = initClients(nodes);
        logger.info("Expected distribution: {}", clientToTx);
        List<DamlClient> clients = assignClients(clientToTx);

        Random random = new Random();
        long startTime = System.nanoTime();

        logger.info("Starting transaction ...");

        for (int i = 0; i < clients.size(); i++) {
            int iouAmount = random.nextInt(10000) + 1;
            Iou iou = new Iou("Alice", "Alice", "AliceCoin", new BigDecimal(iouAmount), emptyList());
            logger.trace("{}", iou);
            Command command = iou.create();
            DamlClient client = clients.get(i);
            client.submitIou(command, party);

            if (rateControl != 0) {
                long timeToSleep = 1_000_000_000 / rateControl;
                Utils.applyRateControl(timeToSleep, i, startTime);
            }
        }

        long endTime = System.nanoTime();
        summarize(endTime - startTime, clientToTx.keySet());
    }

    /**
     * Create client for each node and determine transaction count for it.
     */
    private Map<DamlClient, Long> initClients(List<Node> nodes) {
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
     * Summarize the workload.
     */
    private void summarize(long totalTime, Collection<DamlClient> clients) {
        long timeElaps = totalTime / 1_000_000;
        logger.info("Total time taken: {} ms", timeElaps);
        logger.info("Average latency: {} ms", totalTime / (numOfTransactions * 1_000_000));
        logger.info("Throughput: {} tps", round((numOfTransactions * 1.0 / totalTime) * 1_000_000_000));
        logger.info("Nodes: {}", clients.stream().collect(toMap(DamlClient::getLedgerHost, DamlClient::getTxCount)));
    }

}

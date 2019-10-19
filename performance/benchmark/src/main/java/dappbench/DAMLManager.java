package dappbench;

import static java.lang.System.exit;
import static java.util.Collections.emptyList;
import static java.util.Collections.shuffle;
import static java.util.stream.Collectors.summingInt;
import static org.apache.logging.log4j.LogManager.getLogger;

import java.math.BigDecimal;
import java.util.ArrayList;
import java.util.List;
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
        List<DamlClient> clients = createClients(nodes);
        Random random = new Random();
        long startTime = System.nanoTime();

        logger.info("Starting transaction ...");

        for (int i = 0; i < numOfTransactions; i++) {
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
        summarize(endTime - startTime);
    }

    /**
     * Assign client for each transaction based on the configured distribution and
     * shuffle them.
     */
    private List<DamlClient> createClients(List<Node> nodes) {
        validate(nodes);

        List<DamlClient> clients = new ArrayList<>();

        for (Node node : nodes) {
            if (node.getPercentage() == 0) {
                continue;
            }

            DamlClient client = new DamlClient(node.getIp(), ledgerPort);
            client.init();

            int txCount = numOfTransactions * node.getPercentage() / 100;
            logger.info("Node {} will receive {} IOU commands", node.getIp(), txCount);

            while (txCount > 0) {
                clients.add(client);
                txCount--;
            }
        }

        int residualTxCount = numOfTransactions - clients.size();
        if (residualTxCount > 0) {
            logger.info("Assigning {} residual transactions to node {}", residualTxCount, nodes.get(0).getIp());
        }

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
    private void summarize(long totalTime) {
        long timeElaps = totalTime / 1_000_000;
        logger.info("Total time taken: " + timeElaps + " ms");
        logger.info("Average latency: " + totalTime / (numOfTransactions * 1_000_000) + " ms");
        logger.info("Throughput: " + (numOfTransactions * 1.0 / totalTime) * 1_000_000_000 + " tps");
    }

}

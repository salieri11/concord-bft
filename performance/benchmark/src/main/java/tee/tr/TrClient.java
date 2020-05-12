package tee.tr;

import bench.Node;
import org.apache.logging.log4j.Logger;
import tee.tr.TrStats.GlobalStats;
import tee.tr.TrStats.NodeStats;

import java.time.Instant;
import java.util.List;
import java.util.Map;

import static java.time.Duration.between;
import static java.time.Instant.now;
import static java.util.stream.Collectors.toList;
import static org.apache.logging.log4j.LogManager.getLogger;

/**
 * A thin replica client which subscribes to data updates from one of the nodes and hash updates from all other nodes.
 *
 * @see <a href="https://confluence.eng.vmware.com/pages/viewpage.action?spaceKey=BLOC&title=Thin+Replica</a>
 */
public class TrClient implements TrEventHandler {

    private static final Logger logger = getLogger(TrClient.class);

    private final List<TrSubscriber> subscribers;
    private final GlobalStats globalStats;

    public TrClient(List<Node> nodes) {
        subscribers = nodes.stream().map(node -> new TrSubscriber(node, this)).collect(toList());
        globalStats = new GlobalStats();
    }

    public void start() {
        // Read finite stream to get current state.
        Instant start = now();
        subscribers.forEach(TrSubscriber::readState);
        logger.info("Total duration to read initial state: {}", between(start, now()));

        // The first node subscribes for data, others for hash.
        subscribers.get(0).setDataSubscriber(true);

        // Start subscribing for endless stream of updates
        globalStats.start();
        subscribers.forEach(TrSubscriber::subscribe);
    }

    public void stop(Map<String, Object> jsonStats) {
        subscribers.forEach(TrSubscriber::unsubscribe);
        globalStats.stop();

        logger.info("Total duration of subscription: {}", globalStats.getDuration());
        logger.info("Total data updates received: {}", globalStats.getUpdateCount());
        logger.info("Throughput: {} ups", globalStats.getUpdatesPerSecond());
        logger.info("Average data update time: {} ms", globalStats.getAvgUpdateTimeMillis());

        List<NodeStats> nodeStats = subscribers.stream().map(TrSubscriber::getNodeStats).collect(toList());
        TrStats stats = new TrStats(globalStats, nodeStats);
        jsonStats.put("TR", stats);
    }

    @Override
    public void onDataReceived(long blockId, long updateTimeMillis) {
        globalStats.incrementUpdateCount();
        globalStats.addUpdateTimeMillis(updateTimeMillis);
    }

    @Override
    public void onHashReceived(long blockId) {
        // A real TR would trust data received from one node if other f nodes corroborate the hash.
        // This check is not done here to avoid complexity.
    }

}

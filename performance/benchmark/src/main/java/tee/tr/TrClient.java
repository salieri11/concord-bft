package tee.tr;

import bench.Node;
import org.apache.logging.log4j.Logger;

import java.time.Instant;
import java.util.List;
import java.util.concurrent.atomic.AtomicLong;

import static java.time.Duration.between;
import static java.time.Instant.now;
import static java.util.stream.Collectors.toList;
import static org.apache.logging.log4j.LogManager.getLogger;

/**
 * A thin replica client which subscribes to data updates from one of the nodes and hash updates from all the nodes.
 *
 * @see <a href="https://confluence.eng.vmware.com/pages/viewpage.action?spaceKey=BLOC&title=Thin+Replica</a>
 */
public class TrClient implements TrEventHandler {

    private static final Logger logger = getLogger(TrClient.class);

    private final List<TrSubscriber> subscribers;

    private final AtomicLong totalDataUpdates;
    private final AtomicLong totalResponseTimeMillis;
    private Instant start;

    public TrClient(List<Node> nodes) {
        subscribers = nodes.stream().map(node -> new TrSubscriber(node, this)).collect(toList());
        totalResponseTimeMillis = new AtomicLong();
        totalDataUpdates = new AtomicLong();
    }

    public void start() {
        // Read finite stream to get current state.
        start = now();
        subscribers.forEach(TrSubscriber::readState);
        logger.info("Total duration to read initial state: {}", between(start, now()));

        // The first node subscribes for data, others for hash.
        subscribers.get(0).setDataSubscriber(true);

        // Start subscribing for endless stream of updates
        start = now();
        subscribers.forEach(Thread::start);
    }

    public void stop() {
        // TODO: Unsubscription is not yet supported.
        subscribers.forEach(TrSubscriber::summarize);
        logger.info("Total duration of subscription: {}", between(start, now()));
        long avgResponseTimeMillis = totalResponseTimeMillis.longValue() / totalDataUpdates.longValue();
        logger.info("No. of blocks for which data updates were received: {}", totalDataUpdates);
        logger.info("Average end-to-end response time: {} ms", avgResponseTimeMillis);
        System.exit(0);
    }

    @Override
    public void onDataReceived(long blockId, Instant sendTime) {
        totalDataUpdates.incrementAndGet();
        Instant receiveTime = now();
        long responseTimeMillis = between(sendTime, receiveTime).toMillis();
        logger.debug("Data for blockId {} received in {} ms", blockId, responseTimeMillis);
        totalResponseTimeMillis.addAndGet(responseTimeMillis);
    }

    @Override
    public void onHashReceived(long blockId) {
        // A real TR would trust data received from one node if other f nodes corroborate the hash.
        // This check is not done here to avoid complexity.
    }

}

package tee.tr;

import bench.Node;
import org.json.JSONPropertyName;

import java.time.Duration;
import java.time.Instant;
import java.util.List;

import static java.lang.Math.max;
import static java.lang.String.format;
import static java.time.Duration.between;
import static java.time.Instant.now;

/**
 * Thin Replica stats.
 */
public class TrStats {

    private final GlobalStats global;
    private final List<NodeStats> nodes;

    public TrStats(GlobalStats global, List<NodeStats> nodes) {
        this.global = global;
        this.nodes = nodes;
    }

    @JSONPropertyName("Global")
    public GlobalStats getGlobal() {
        return global;
    }

    @JSONPropertyName("Nodes")
    public List<NodeStats> getNodes() {
        return nodes;
    }

    /**
     * Node specific stats.
     */
    public static class NodeStats {

        private final String address;

        private final OpStats readState;
        private final OpStats subscription;

        public NodeStats(Node node, OpStats readState, OpStats subscription) {
            address = format("%s:%d", node.getIp(), node.getPort());
            this.readState = readState;
            this.subscription = subscription;
        }

        @JSONPropertyName("Address")
        public String getAddress() {
            return address;
        }

        @JSONPropertyName("ReadState")
        public OpStats getReadState() {
            return readState;
        }

        @JSONPropertyName("Subscription")
        public OpStats getSubscription() {
            return subscription;
        }
    }

    /**
     * Operation specific stats.
     */
    public static class OpStats {

        private Instant start;
        private Instant end;

        private long updateCount;

        public void start() {
            start = now();
        }

        public void stop() {
            end = now();
        }

        @JSONPropertyName("Duration")
        public Duration getDuration() {
            return between(start, end);
        }

        public void incrementUpdateCount() {
            updateCount++;
        }

        @JSONPropertyName("UpdateCount")
        public long getUpdateCount() {
            return updateCount;
        }

        @JSONPropertyName("UpdatesPerSecond")
        public long getUpdatesPerSecond() {
            return updateCount / max(getDuration().getSeconds(), 1);
        }
    }

    /**
     * Overall stats for relevant data.
     */
    public static class GlobalStats extends OpStats {

        private long totalUpdateTimeMillis;

        public void addUpdateTimeMillis(long updateTimeMillis) {
            totalUpdateTimeMillis += updateTimeMillis;
        }

        @JSONPropertyName("AvgUpdateTimeMillis")
        public long getAvgUpdateTimeMillis() {
            return totalUpdateTimeMillis / getUpdateCount();
        }
    }

}

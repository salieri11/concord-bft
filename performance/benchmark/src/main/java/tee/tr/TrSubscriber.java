package tee.tr;

import bench.Node;
import com.google.protobuf.ByteString;
import com.vmware.concord.thin_replica.ThinReplicaGrpc.ThinReplicaBlockingStub;
import com.vmware.concord.thin_replica.ThinReplicaOuterClass.KVPair;
import com.vmware.concord.thin_replica.ThinReplicaOuterClass.ReadStateRequest;
import com.vmware.concord.thin_replica.ThinReplicaOuterClass.SubscriptionRequest;
import io.grpc.ClientInterceptor;
import io.grpc.ManagedChannel;
import io.grpc.Metadata;
import org.apache.logging.log4j.Logger;
import tee.tr.TrStats.NodeStats;
import tee.tr.TrStats.OpStats;

import java.time.Instant;
import java.util.List;

import static com.google.common.base.Preconditions.checkNotNull;
import static com.google.protobuf.ByteString.copyFromUtf8;
import static com.vmware.concord.thin_replica.ThinReplicaGrpc.newBlockingStub;
import static io.grpc.ManagedChannelBuilder.forAddress;
import static io.grpc.Metadata.ASCII_STRING_MARSHALLER;
import static io.grpc.Metadata.Key.of;
import static io.grpc.stub.MetadataUtils.newAttachHeadersInterceptor;
import static java.time.Duration.between;
import static java.time.Instant.now;
import static java.time.Instant.parse;
import static java.util.UUID.randomUUID;
import static org.apache.logging.log4j.LogManager.getLogger;

/**
 * It represents either a data or hash subscription for updates from a blockchain node.
 */
public class TrSubscriber extends Thread {

    private static final Logger logger = getLogger(TrSubscriber.class);

    public static final String TR_ID = randomUUID().toString();
    public static final ByteString SEND_TIME = copyFromUtf8(randomUUID().toString());

    private final ThinReplicaBlockingStub blockingStub;
    private final TrEventHandler eventHandler;

    private long initialBlockId;
    private boolean dataSubscriber;

    private final OpStats readState;
    private final OpStats subscription;
    private final NodeStats nodeStats;


    public TrSubscriber(Node node, TrEventHandler eventHandler) {
        this.eventHandler = eventHandler;

        ManagedChannel channel = forAddress(node.getIp(), node.getPort()).usePlaintext().intercept(interceptor()).build();
        blockingStub = newBlockingStub(channel);

        readState = new OpStats();
        subscription = new OpStats();
        nodeStats = new NodeStats(node, readState, subscription);
    }

    /**
     * Interceptor to add thin replica id as gRPC metadata.
     */
    private ClientInterceptor interceptor() {
        Metadata extraHeaders = new Metadata();
        extraHeaders.put(of("client_id", ASCII_STRING_MARSHALLER), TR_ID);
        return newAttachHeadersInterceptor(extraHeaders);
    }

    /**
     * Flag to indicate whether to subscribe for data or hashes.
     */
    public void setDataSubscriber(boolean dataSubscriber) {
        this.dataSubscriber = dataSubscriber;
    }

    /**
     * Subscribe to an endless stream of updates.
     */
    @Override
    public void run() {
        if (dataSubscriber) {
            subscribeToData();
        } else {
            subscribeToHashes();
        }
    }

    /**
     * ReadState is done just to get the last blockId so that subscription can begin from there.
     * Its performance is not being tested here.
     */
    public void readState() {
        readState.start();
        ReadStateRequest request = ReadStateRequest.newBuilder().build();

        blockingStub.readState(request).forEachRemaining(data -> {
            logger.debug("{}", data);
            initialBlockId = data.getBlockId();
            readState.incrementUpdateCount();
        });

        readState.stop();
        logger.info("{} - {} blocks read in {}", nodeStats.getAddress(), initialBlockId, readState.getDuration());
    }

    /**
     * Subscribe for updates.
     */
    public void subscribe() {
        subscription.start();
        start();
    }

    /**
     * Each thin replica would subscribe for data updates from only one node.
     * We don't save data, hence ignore it.
     */
    private void subscribeToData() {
        SubscriptionRequest request = SubscriptionRequest.newBuilder().setBlockId(initialBlockId).build();

        blockingStub.subscribeToUpdates(request).forEachRemaining(data -> {
            // Hack: We ignore the blockId from which we start subscription.
            if (data.getBlockId() == initialBlockId) {
                return;
            }

            logger.debug("{}", data);

            subscription.incrementUpdateCount();

            // BlockId without data implies that the update is not relevant for this TR.
            if (data.getDataCount() == 0) {
                return;
            }

            List<KVPair> pairs = data.getDataList();
            // Special key to help TR calculate end-to-end time
            Instant sendTime = null;
            for (KVPair pair : pairs) {
                ByteString key = pair.getKey();
                if (key.equals(SEND_TIME)) {
                    sendTime = parse(pair.getValue().toStringUtf8());
                    break;
                }
            }

            checkNotNull(sendTime, "Could not find start time");
            long responseTimeMillis = between(sendTime, now()).toMillis();
            logger.debug("Data for blockId {} received in {} ms", data.getBlockId(), responseTimeMillis);

            eventHandler.onDataReceived(data.getBlockId(), responseTimeMillis);
        });
    }

    /**
     * Each thin replica would subscribe for hash updates from all the nodes.
     * We don't match hashes across nodes, hence ignore it.
     */
    private void subscribeToHashes() {
        SubscriptionRequest request = SubscriptionRequest.newBuilder().setBlockId(initialBlockId).build();

        blockingStub.subscribeToUpdateHashes(request).forEachRemaining(hash -> {
            // Hack: We ignore the blockId from which we start subscription.
            if (hash.getBlockId() == initialBlockId) {
                return;
            }

            logger.debug("{}", hash);

            subscription.incrementUpdateCount();
            eventHandler.onHashReceived(hash.getBlockId());
        });
    }

    /**
     * TODO: Unsubscription is not yet supported.
     */
    public void unsubscribe() {
        subscription.stop();
        logger.info("{} - {} updates received in {}", nodeStats.getAddress(), subscription.getUpdateCount(), subscription.getDuration());
    }

    /**
     * TR stats for this node.
     */
    public NodeStats getNodeStats() {
        return nodeStats;
    }
}

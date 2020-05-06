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

import java.time.Instant;
import java.util.List;
import java.util.concurrent.atomic.LongAdder;

import static com.google.protobuf.ByteString.copyFromUtf8;
import static com.vmware.concord.thin_replica.ThinReplicaGrpc.newBlockingStub;
import static io.grpc.ManagedChannelBuilder.forAddress;
import static io.grpc.Metadata.ASCII_STRING_MARSHALLER;
import static io.grpc.Metadata.Key.of;
import static io.grpc.stub.MetadataUtils.newAttachHeadersInterceptor;
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

    private final Node node;
    private final ThinReplicaBlockingStub blockingStub;
    private final TrEventHandler eventHandler;
    private final LongAdder updateCount;

    private long initialBlockId;
    private boolean dataSubscriber;


    public TrSubscriber(Node node, TrEventHandler eventHandler) {
        this.node = node;
        this.eventHandler = eventHandler;
        updateCount = new LongAdder();

        ManagedChannel channel = forAddress(node.getIp(), node.getPort()).usePlaintext().intercept(interceptor()).build();
        blockingStub = newBlockingStub(channel);
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
        ReadStateRequest request = ReadStateRequest.newBuilder().build();

        blockingStub.readState(request).forEachRemaining(data -> {
            logger.debug("{}", data);
            initialBlockId = data.getBlockId();
        });

        logger.info("{} - Initial blockId: {}", node, initialBlockId);
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

            List<KVPair> pairs = data.getDataList();

            // BlockId without data implies that the update is not relevant for this TR.
            if (pairs == null) {
                return;
            }

            // Special key to help TR calculate end-to-end time
            Instant sendTime = null;
            for (KVPair pair : pairs) {
                ByteString key = pair.getKey();
                if (key.equals(SEND_TIME)) {
                    sendTime = parse(pair.getValue().toStringUtf8());
                    break;
                }
            }

            updateCount.increment();
            eventHandler.onDataReceived(data.getBlockId(), sendTime);
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

            updateCount.increment();
            eventHandler.onHashReceived(hash.getBlockId());
        });
    }

    public void summarize() {
        logger.info("{} - Update count: {}", node, updateCount);
    }

}

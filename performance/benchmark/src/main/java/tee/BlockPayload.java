package tee;

import com.google.protobuf.ByteString;
import com.vmware.concord.tee.Tee.KVData;
import com.vmware.concord.tee.Tee.TridKV;
import org.apache.logging.log4j.Logger;

import java.util.List;
import java.util.stream.Stream;

import static com.google.protobuf.ByteString.copyFromUtf8;
import static java.time.Instant.now;
import static java.util.UUID.randomUUID;
import static java.util.stream.Collectors.toList;
import static org.apache.logging.log4j.LogManager.getLogger;
import static tee.tr.TrSubscriber.SEND_TIME;
import static tee.tr.TrSubscriber.TR_ID;

/**
 * Payload factory for TEE WriteBlock protocol.
 */
public class BlockPayload {

    private static final Logger logger = getLogger(BlockPayload.class);

    // Size used to determine how many KV pairs should be created.
    private static final int UUID_SIZE = 36;
    private static final int TRID_KV_SIZE = UUID_SIZE * 2 + TR_ID.length() + 6;

    private final int kvCount;
    private final ByteString value;

    public BlockPayload(int requestSize) {
        kvCount = requestSize / TRID_KV_SIZE;
        logger.info("Size of each request: {} bytes", requestSize);
        logger.info("KV pairs in each request: {}", kvCount);

        // Variation in value is immaterial, hence shared across all the keys.
        value = toByteString(randomUUID());
    }

    /**
     * Add enough KV pairs to create data of configured size.
     * Add a special key to help TR calculate end-to-end time.
     */
    public KVData create() {
        List<TridKV> pairs = Stream.generate(this::createRandomKv).limit(kvCount).collect(toList());
        pairs.add(createKv(SEND_TIME, toByteString(now())));
        return KVData.newBuilder().addAllTridKvs(pairs).build();
    }

    /**
     * Create a random KV pair.
     */
    private TridKV createRandomKv() {
        return createKv(toByteString(randomUUID()), value);
    }

    /**
     * Create the given KV pair.
     */
    private TridKV createKv(ByteString key, ByteString value) {
        return TridKV.newBuilder().addTrids(TR_ID).setKeyBytes(key).setValueBytes(value).build();
    }

    /**
     * Utility to create ByteString.
     */
    private ByteString toByteString(Object value) {
        return copyFromUtf8(value.toString());
    }
}

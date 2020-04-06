package tee;

import org.apache.logging.log4j.Logger;

import java.nio.ByteBuffer;
import java.util.Random;

import static java.nio.ByteBuffer.allocate;
import static java.nio.ByteOrder.LITTLE_ENDIAN;
import static java.util.stream.IntStream.range;
import static org.apache.logging.log4j.LogManager.getLogger;

/**
 * Payload factory for TEE/skvbcWrite protocol.
 *
 * @see <a href="https://github.com/vmware/concord-bft/tree/master/tests/apollo</a>
 * <p>
 * Structure:
 * type - 1 byte
 * read_version - 8 bytes
 * num_of_reads - 8 bytes
 * num_of_updates - 8 bytes
 * {num_of_updates} of keyval sequences (key and value are 21 bytes each)
 * </p>
 */
public class SkvbcPayload {

    private static final Logger logger = getLogger(SkvbcPayload.class);

    /**
     * Note: All sizes are in bytes.
     */
    private static final int META_SIZE = 25;
    private static final int KEY_SIZE = 21;
    private static final int VALUE_SIZE = 21;
    private static final int KV_SIZE = KEY_SIZE + VALUE_SIZE;

    private final int kvCount;
    private final int adjustedTotalSize;

    private final byte[] value;

    public SkvbcPayload(int configuredTotalSize) {
        kvCount = (configuredTotalSize - META_SIZE) / KV_SIZE;
        adjustedTotalSize = (kvCount * KV_SIZE) + META_SIZE;

        logger.info("KV pairs in each request: {}", kvCount);
        logger.info("Configured size of each request: {} bytes", configuredTotalSize);
        logger.info("Actual size of each request: {} bytes", adjustedTotalSize);

        // Variation in value is immaterial, hence shared across all the keys.
        value = new byte[VALUE_SIZE];
    }

    /**
     * Create payload.
     */
    public byte[] create() {
        ByteBuffer buf = allocate(adjustedTotalSize).order(LITTLE_ENDIAN);

        buf.put((byte) 2 /* write */);
        buf.putLong(0 /* read version */);
        buf.putLong(0  /* read size */);
        buf.putLong(kvCount * KV_SIZE);

        Random random = new Random();
        byte[] key = new byte[KEY_SIZE];

        range(0, kvCount).forEach(i -> {
            random.nextBytes(key);

            buf.put(key);
            buf.put(value);
        });

        return buf.array();
    }

}

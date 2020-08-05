package pee;

import com.vmware.concord.performance.Performance.*;
import com.vmware.concord.performance.PerformanceServiceGrpc.PerformanceServiceBlockingStub;
import org.apache.logging.log4j.Logger;

import java.util.List;
import java.util.Random;

import static java.lang.Integer.parseInt;
import static java.util.concurrent.ThreadLocalRandom.current;
import static org.apache.logging.log4j.LogManager.getLogger;
import com.google.protobuf.ByteString;

/**
 * PEE PerfWrite operation
 */
public class PerfWrite {

    private static final Logger logger = getLogger(PerfWrite.class);

    private final int kvCount;
    private final int keySize;
    private final int valueSize;
    private final int batchSize;
    private final int payloadSize;

    private final Random randomNo;
    private final byte[] payload;

    private final PerformanceServiceBlockingStub blockingStub;

    private String initId;

    public PerfWrite(List<String> params, PerformanceServiceBlockingStub blockingStub) {
        kvCount = parseInt(params.get(2));
        keySize = parseInt(params.get(3));
        valueSize = parseInt(params.get(4));
        batchSize = parseInt(params.get(5));
        payloadSize = parseInt(params.get(6));

        // Create payload
        randomNo = new Random();
        payload = new byte[payloadSize];
        randomNo.nextBytes(payload);

        this.blockingStub = blockingStub;
    }

    /**
     * Create blocks in Concord memory.
     */
    public void init() {
        if (isPreCreate()) {
            PerfInitRequest request = PerfInitRequest.newBuilder()
                    .setBlockCount(batchSize)
                    .setKvCount(kvCount)
                    .setKeySize(keySize)
                    .setValueSize(valueSize)
                    .build();

            PerfInitResponse response = blockingStub.perfInit(request);
            logger.info("{}", response.getWorksetInfo());

            initId = response.getId();
            logger.info("Work set (blockCount, kvCount, kSize, vSize): {}", initId);
        }
    }

    /**
     * Send request to Concord to write a block based on the given source.
     */
    public void write() {
        PerfWriteRequest request = isPreCreate() ? referAndWriteRequest() : createAndWriteRequest();
        PerfWriteResponse response = blockingStub.perfWrite(request);
        logger.debug("Message: {}, BlockId: {}", response.getMessage(), response.getNewBlockId());
    }

    /**
     * A pre-created block in Concord memory is referred and written.
     */
    private PerfWriteRequest referAndWriteRequest() {
        PerfWriteFromInit source = PerfWriteFromInit.newBuilder().setInitId(initId).build();
        return PerfWriteRequest.newBuilder()
        .setFromInit(source)
        .setPayload(ByteString.copyFrom(payload))
        .build();
    }

    /**
     * A block is created in Concord memory and written.
     */
    private PerfWriteRequest createAndWriteRequest() {
        PerfWriteExternal source = PerfWriteExternal.newBuilder()
                .setKeyPrefix(current().nextInt() + 1)
                .setValPrefix(current().nextInt() + 1)
                .setKvCount(kvCount)
                .setKeySize(keySize)
                .setValueSize(valueSize)
                .build();

        return PerfWriteRequest.newBuilder()
            .setExternal(source)
            .setPayload(ByteString.copyFrom(payload))
            .build();
    }

    /**
     * Cleanup blocks in Concord memory.
     */
    public void clear() {
        if (isPreCreate()) {
            PerfCleanRequest request = PerfCleanRequest.newBuilder().build();
            PerfCleanResponse response = blockingStub.perfClear(request);
            logger.info("{}", response.getMessage());
        }
    }

    /**
     * A positive value implies pre-creation of blocks in Concord memory.
     */
    private boolean isPreCreate() {
        return batchSize > 0;
    }
}

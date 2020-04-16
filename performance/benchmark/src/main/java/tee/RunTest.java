package tee;

import com.vmware.concord.tee.Tee.TestInput;
import com.vmware.concord.tee.Tee.TestOutput;
import com.vmware.concord.tee.TeeServiceGrpc.TeeServiceBlockingStub;
import org.apache.logging.log4j.Logger;

import static com.vmware.concord.tee.Tee.TestInput.newBuilder;
import static org.apache.logging.log4j.LogManager.getLogger;


/**
 * TEE runTest operation
 */
public class RunTest implements Operation {

    private static final Logger logger = getLogger(RunTest.class);

    private final TeeServiceBlockingStub blockingStub;
    private final RunTestPayload payload;

    public RunTest(int requestSize, TeeServiceBlockingStub blockingStub) {
        this.payload = new RunTestPayload(requestSize);
        this.blockingStub = blockingStub;
    }

    @Override
    public void execute() {
        TestInput testInput = newBuilder().setTestInput(payload.create()).build();
        TestOutput response = blockingStub.runTest(testInput);
        logger.debug("Response size (bytes): {}", response.getTestOutput().length());
    }

    /**
     * Payload factory for TEE/runTest protocol.
     * Variation in payload is immaterial for this test, hence this implementation uses the same payload each request.
     */
    private static class RunTestPayload {

        private static final Logger logger = getLogger(RunTestPayload.class);

        private final String data;

        private RunTestPayload(int requestSize) {
            data = new String(new byte[requestSize]);
            logger.info("Size of each request: {} bytes", data.length());
        }

        /**
         * Create payload (always the same payload).
         */
        public String create() {
            return data;
        }
    }
}

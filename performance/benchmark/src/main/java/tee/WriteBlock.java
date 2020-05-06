package tee;

import com.vmware.concord.tee.Tee.KVData;
import com.vmware.concord.tee.Tee.TestOutput;
import com.vmware.concord.tee.TeeServiceGrpc.TeeServiceBlockingStub;
import org.apache.logging.log4j.Logger;

import java.util.regex.Matcher;
import java.util.regex.Pattern;

import static java.lang.Integer.parseInt;
import static java.util.regex.Pattern.compile;
import static org.apache.logging.log4j.LogManager.getLogger;

/**
 * TEE WriteBlock operation
 */
public class WriteBlock implements Operation {

    private static final Logger logger = getLogger(WriteBlock.class);
    private static final Pattern pattern = compile("tee: new block (\\d+)");

    private final BlockPayload payload;
    private final TeeServiceBlockingStub blockingStub;

    public WriteBlock(int requestSize, TeeServiceBlockingStub blockingStub) {
        this.payload = new BlockPayload(requestSize);
        this.blockingStub = blockingStub;
    }

    @Override
    public void execute() {
        KVData request = payload.create();
        logger.debug("Serialized size: {}", request.getSerializedSize());
        TestOutput response = blockingStub.writeBlock(request);
        String output = response.getTestOutput();

        Matcher m = pattern.matcher(output);
        if (m.matches()) {
            int blockId = parseInt(m.group(1));
            logger.debug("BlockId [{}] created", blockId);
        } else {
            logger.warn("Unexpected output: {}", output);
        }
    }
}

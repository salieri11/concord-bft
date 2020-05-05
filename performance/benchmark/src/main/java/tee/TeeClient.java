package tee;

import com.vmware.concord.tee.TeeServiceGrpc.TeeServiceBlockingStub;
import dappbench.WorkloadClient;
import io.grpc.ManagedChannel;
import org.apache.logging.log4j.Logger;

import static com.vmware.concord.tee.TeeServiceGrpc.newBlockingStub;
import static io.grpc.ManagedChannelBuilder.forAddress;
import static org.apache.logging.log4j.LogManager.getLogger;

/**
 * Client for TEE
 *
 * @see <a href="https://gitlab.eng.vmware.com/blockchain/vmwathena_blockchain/blob/master/concord/proto/tee.proto</a>
 */
public class TeeClient extends WorkloadClient {

    private static final Logger logger = getLogger(TeeClient.class);

    private final OperationType opType;
    private final int requestSize;

    private Operation operation;

    public TeeClient(String concordHost, int concordPort, OperationType opType, int requestSize) {
        super(concordHost, concordPort);
        this.opType = opType;
        this.requestSize = requestSize;
    }

    /**
     * Initialize the connection and payload.
     */
    public void init() {
        ManagedChannel channel = forAddress(getHost(), getPort()).usePlaintext().build();
        TeeServiceBlockingStub blockingStub = newBlockingStub(channel);

        switch (opType) {
            case RUN_TEST:
                operation = new RunTest(requestSize, blockingStub);
                break;
            case SKVBC_WRITE:
                operation = new SkvbcWrite(requestSize, blockingStub);
                break;
            case WRITE_BLOCK:
                operation = new WriteBlock(requestSize, blockingStub);
                break;
            default:
                logger.error("Unknown TEE operation: " + opType);
        }
    }

    @Override
    protected void doExecute() {
        operation.execute();
    }

}

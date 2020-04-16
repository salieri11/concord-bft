package tee;

import com.vmware.concord.tee.TeeServiceGrpc;
import com.vmware.concord.tee.TeeServiceGrpc.TeeServiceBlockingStub;
import dappbench.WorkloadClient;
import io.grpc.ManagedChannel;
import org.apache.logging.log4j.Logger;

import static io.grpc.ManagedChannelBuilder.forAddress;
import static org.apache.logging.log4j.LogManager.getLogger;

/**
 * Client for TEE
 *
 * @see <a href="https://gitlab.eng.vmware.com/blockchain/vmwathena_blockchain/blob/master/concord/proto/tee.proto</a>
 */
public class TeeClient extends WorkloadClient {

    private static final Logger logger = getLogger(TeeClient.class);

    private static final String RUN_TEST = "RunTest";
    private static final String SKVBC_WRITE = "SkvbcWrite";

    private final String operationName;
    private final int requestSize;

    private Operation operation;

    public TeeClient(String concordHost, int concordPort, String operationName, int requestSize) {
        super(concordHost, concordPort);
        this.operationName = operationName;
        this.requestSize = requestSize;
    }

    /**
     * Initialize the connection and payload.
     */
    public void init() {
        ManagedChannel channel = forAddress(getHost(), getPort()).usePlaintext().build();
        TeeServiceBlockingStub blockingStub = TeeServiceGrpc.newBlockingStub(channel);

        switch (operationName) {
            case RUN_TEST:
                operation = new RunTest(requestSize, blockingStub);
                break;
            case SKVBC_WRITE:
                operation = new SkvbcWrite(requestSize, blockingStub);
                break;
            default:
                logger.error("Unknown TEE operation: " + operationName);
                throw new IllegalArgumentException("No such operation: " + operationName);
        }
    }

    @Override
    protected void doExecute() {
        operation.execute();
    }

}

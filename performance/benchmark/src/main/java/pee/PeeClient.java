package pee;

import com.vmware.concord.performance.PerformanceServiceGrpc.PerformanceServiceBlockingStub;
import dappbench.WorkloadClient;
import io.grpc.ManagedChannel;

import java.util.List;

import static com.vmware.concord.performance.PerformanceServiceGrpc.newBlockingStub;
import static io.grpc.ManagedChannelBuilder.forAddress;

/**
 * Client for PEE. As of now, PerfWrite is the sole operation.
 *
 * @see <a href="https://gitlab.eng.vmware.com/blockchain/vmwathena_blockchain/blob/master/concord/proto/performance.proto</a>
 */

public class PeeClient extends WorkloadClient {

    private List<String> params;
    private PerfWrite perfWrite;

    public PeeClient(String host, int port, List<String> params) {
        super(host, port);
        this.params = params;
    }

    @Override
    protected void init() {
        ManagedChannel channel = forAddress(getHost(), getPort()).usePlaintext().build();
        PerformanceServiceBlockingStub blockingStub = newBlockingStub(channel);
        perfWrite = new PerfWrite(params, blockingStub);
        perfWrite.init();
    }

    @Override
    protected void doExecute() {
        perfWrite.write();
    }

    @Override
    protected void cleanup() {
        perfWrite.clear();
    }
}

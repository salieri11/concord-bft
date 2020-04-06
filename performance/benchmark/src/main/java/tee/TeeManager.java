package tee;

import bench.AdvancedConfig;
import bench.SimpleConfig;
import bench.Workload;
import com.wavefront.sdk.common.WavefrontSender;
import dappbench.WorkloadClient;
import dappbench.WorkloadManager;

import java.util.List;
import java.util.Optional;

import static java.lang.Integer.parseInt;

/**
 * Factory for TEE client
 */
public class TeeManager extends WorkloadManager {

    private final String operationName;
    private final int numOfRequests;
    private final int sizeOfRequest;

    public TeeManager(Workload workload, SimpleConfig simpleconfig, AdvancedConfig advancedConfig, Optional<WavefrontSender> optionalWavefrontSender) {
        super(workload, simpleconfig, advancedConfig, optionalWavefrontSender);

        List<String> params = workload.getParams();
        this.operationName = params.get(0);
        this.numOfRequests = parseInt(params.get(1));
        this.sizeOfRequest = parseInt(params.get(2));
    }

    @Override
    protected int getRequestCount() {
        return numOfRequests;
    }

    @Override
    protected WorkloadClient createClient(String host, int port) {
        TeeClient client = new TeeClient(host, port, operationName, sizeOfRequest);
        client.init();
        return client;
    }
}

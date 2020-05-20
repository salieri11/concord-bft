package pee;

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
 * Factory for PEE client
 */
public class PeeManager extends WorkloadManager {

    private final String operation;
    private final int numOfRequests;
    private final List<String> params;

    public PeeManager(Workload workload, SimpleConfig simpleconfig, AdvancedConfig advancedConfig, Optional<WavefrontSender> optionalWavefrontSender) {
        super(workload, simpleconfig, advancedConfig, optionalWavefrontSender);

        params = workload.getParams();
        operation = params.get(0);
        numOfRequests = parseInt(params.get(1));
    }

    @Override
    protected int getRequestCount() {
        return numOfRequests;
    }

    @Override
    protected WorkloadClient createClient(String host, int port) {
        return new PeeClient(host, port, params);
    }

    @Override
    protected String getOperationType() {
        return operation;
    }

}

package dappbench;

import bench.AdvancedConfig;
import bench.SimpleConfig;
import bench.Workload;
import com.wavefront.sdk.common.WavefrontSender;

import java.util.List;
import java.util.Optional;

import static java.lang.Integer.parseInt;

/**
 * Factory for DAML client
 */
public class DAMLManager extends WorkloadManager {

    private final String party;
    private final int numOfRequests;
    private final int noOfCommandsPerTransaction;
    private final boolean logging;

    public DAMLManager(Workload workload, SimpleConfig simpleconfig, AdvancedConfig advancedConfig, Optional<WavefrontSender> optionalWavefrontSender) {
        super(workload, simpleconfig, advancedConfig, optionalWavefrontSender);

        List<String> params = workload.getParams();
        this.party = params.get(0);
        this.numOfRequests = parseInt(params.get(1));
        this.noOfCommandsPerTransaction = parseInt(params.get(2));

        this.logging = workload.getLogging();
    }

    @Override
    protected int getRequestCount() {
        return numOfRequests;
    }

    @Override
    protected String getOperationType() {
        return "SubmitAndWait";
    }

    @Override
    protected WorkloadClient createClient(String host, int port) {
        return new DamlClient(host, port, party, noOfCommandsPerTransaction, logging);
    }


}

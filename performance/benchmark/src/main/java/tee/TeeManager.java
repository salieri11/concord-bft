package tee;

import bench.AdvancedConfig;
import bench.Node;
import bench.SimpleConfig;
import bench.Workload;
import com.wavefront.sdk.common.WavefrontSender;
import dappbench.WorkloadClient;
import dappbench.WorkloadManager;
import tee.tr.TrClient;

import java.util.List;
import java.util.Optional;

import static com.google.common.base.Preconditions.checkArgument;
import static java.lang.Integer.parseInt;
import static tee.TeeClient.WRITE_BLOCK;

/**
 * Factory for TEE client
 */
public class TeeManager extends WorkloadManager {

    private final String operationName;
    private final int numOfRequests;
    private final int sizeOfRequest;
    private final List<String> params;

    private TrClient thinReplica;

    public TeeManager(Workload workload, SimpleConfig simpleconfig, AdvancedConfig advancedConfig, Optional<WavefrontSender> optionalWavefrontSender) {
        super(workload, simpleconfig, advancedConfig, optionalWavefrontSender);

        params = workload.getParams();
        this.operationName = params.get(0);
        this.numOfRequests = parseInt(params.get(1));
        this.sizeOfRequest = parseInt(params.get(2));
    }

    @Override
    public void executeWorkload() throws Exception {
        // Use thin replica for this operation
        if (operationName.equals(WRITE_BLOCK)) {
            List<Node> nodes = getNodes();
            int f = parseInt(params.get(3));
            checkArgument(nodes.size() == 3 * f + 1, "Total nodes [%s] != 3f+1 for f=%s", nodes.size(), f);
            thinReplica = new TrClient(nodes);
            thinReplica.start();
        }

        super.executeWorkload();

        if (thinReplica != null) {
            thinReplica.stop();
        }
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

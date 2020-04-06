package dappbench;

import io.grpc.Status;
import io.grpc.StatusRuntimeException;
import org.apache.logging.log4j.Logger;

import java.util.HashMap;
import java.util.Map;
import java.util.concurrent.atomic.LongAdder;

import static io.grpc.Status.OK;
import static org.apache.logging.log4j.LogManager.getLogger;

/**
 * Basic interface for workload specific clients.
 */
public abstract class WorkloadClient {

    private static final Logger logger = getLogger(WorkloadClient.class);

    private final String host;
    private final int port;

    private final Map<Status.Code, LongAdder> statusToCount;

    private boolean logError;

    public WorkloadClient(String host, int port) {
        this.host = host;
        this.port = port;

        statusToCount = new HashMap<>();
        logError = true;
    }

    /**
     * Execute the workload
     */
    public final void execute() {
        try {
            doExecute();
            increment(OK);
        } catch (RuntimeException e) {
            Status status = getStatus(e);
            increment(status);
            // Avoiding error message flooding in terminal
            if (logError) {
                logger.warn(e.getMessage());
                logError = false;
            }
        }
    }

    /**
     * Get response status which might be wrapped as nested exception.
     */
    private Status getStatus(Throwable e) {
        if (e instanceof StatusRuntimeException) {
            return ((StatusRuntimeException) e).getStatus();
        }

        if (e.getCause() != null) {
            return getStatus(e.getCause());
        }

        // Default status if can't be determined.
        return Status.UNKNOWN;
    }

    /**
     * Increment the count of the given status.
     */
    private void increment(Status status) {
        statusToCount.putIfAbsent(status.getCode(), new LongAdder());
        statusToCount.get(status.getCode()).increment();
    }

    /**
     * Workload specific execution.
     * Implementation should report gPPC status code wrapped in RuntimeException.
     */
    protected abstract void doExecute();

    /**
     * Size of ledger
     */
    protected abstract long getLedgerSize();

    /**
     * No. of requests grouped by status
     */
    public final Map<Status.Code, LongAdder> getStatusCount() {
        return statusToCount;
    }

    /**
     * Target hostname
     */
    public final String getHost() {
        return host;
    }

    /**
     * Target port
     */
    public final int getPort() {
        return port;
    }

    /**
     * Combination of host and port uniquely identifies a client.
     */
    @Override
    public String toString() {
        return String.format("%s:%d", host, port);
    }
}

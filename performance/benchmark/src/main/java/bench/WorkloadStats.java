package bench;

import io.grpc.Status.Code;
import org.json.JSONPropertyName;

import java.time.Duration;
import java.util.Map;
import java.util.concurrent.atomic.LongAdder;

/**
 * Workload stats of the test run.
 */
public class WorkloadStats {

    private String operation;
    private int concurrency;
    private int rateControl;
    private Duration testDuration;
    private long rps;
    private long averageResponseTimeMillis;
    private int totalRequests;
    private Map<String, Map<Code, LongAdder>> responseStatus;

    @JSONPropertyName("Operation")
    public String getOperation() {
        return operation;
    }

    public void setOperation(String operation) {
        this.operation = operation;
    }

    @JSONPropertyName("Concurrency")
    public int getConcurrency() {
        return concurrency;
    }

    public void setConcurrency(int concurrency) {
        this.concurrency = concurrency;
    }

    @JSONPropertyName("RateControl")
    public int getRateControl() {
        return rateControl;
    }

    public void setRateControl(int rateControl) {
        this.rateControl = rateControl;
    }

    @JSONPropertyName("Duration")
    public Duration getTestDuration() {
        return testDuration;
    }

    public void setTestDuration(Duration testDuration) {
        this.testDuration = testDuration;
    }

    @JSONPropertyName("RequestsPerSecond")
    public long getRps() {
        return rps;
    }

    public void setRps(long rps) {
        this.rps = rps;
    }

    @JSONPropertyName("AvgResponseTimeMillis")
    public long getAverageResponseTimeMillis() {
        return averageResponseTimeMillis;
    }

    public void setAverageResponseTimeMillis(long averageResponseTimeMillis) {
        this.averageResponseTimeMillis = averageResponseTimeMillis;
    }

    @JSONPropertyName("TotalRequests")
    public int getTotalRequests() {
        return totalRequests;
    }

    public void setTotalRequests(int totalRequests) {
        this.totalRequests = totalRequests;
    }

    @JSONPropertyName("ResponseStatus")
    public Map<String, Map<Code, LongAdder>> getResponseStatus() {
        return responseStatus;
    }

    public void setResponseStatus(Map<String, Map<Code, LongAdder>> responseStatus) {
        this.responseStatus = responseStatus;
    }

}


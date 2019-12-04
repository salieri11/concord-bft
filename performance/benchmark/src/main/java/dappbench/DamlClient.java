package dappbench;

import com.daml.ledger.javaapi.data.Command;
import com.daml.ledger.javaapi.data.LedgerOffset;
import com.daml.ledger.javaapi.data.LedgerOffset.Absolute;
import com.daml.ledger.rxjava.DamlLedgerClient;
import org.apache.logging.log4j.Logger;

import java.time.Instant;
import java.util.List;
import java.util.concurrent.atomic.LongAdder;

import static com.daml.ledger.rxjava.DamlLedgerClient.forHostWithLedgerIdDiscovery;
import static java.lang.Long.parseLong;
import static java.time.Instant.now;
import static java.util.Collections.singletonList;
import static java.util.Optional.empty;
import static java.util.UUID.randomUUID;
import static org.apache.logging.log4j.LogManager.getLogger;

/**
 * Client for a DAML ledger API. It uses the code from IOU quick start DApp.
 * <p>
 * {@link https://docs.daml.com/getting-started/quickstart.html#download-the-quickstart-application}
 */
public class DamlClient {

    private final static Logger logger = getLogger(DamlClient.class);

    private final String ledgerHost;
    private final int ledgerPort;

    private DamlLedgerClient client;
    private LongAdder txCount;

    /**
     * Constructor.
     */
    public DamlClient(String ledgerHost, int ledgerPort) {
        this.ledgerHost = ledgerHost;
        this.ledgerPort = ledgerPort;
    }

    /**
     * Connects to the ledger and runs initial validation.
     */
    public void init() {
        client = forHostWithLedgerIdDiscovery(ledgerHost, ledgerPort, empty());
        client.connect();
        String ledgerId = client.getLedgerId();
        logger.debug("ledger-id for node {} is {}", ledgerHost, ledgerId);
        txCount = new LongAdder();
    }

    /**
     * Get offset at end of the ledger.
     */
    public long getCurrentLedgerOffset() {
        LedgerOffset offset = client.getTransactionsClient().getLedgerEnd().blockingGet();
        return parseLong(((Absolute) offset).getOffset());
    }

    /**
     * Submit IOU creation.
     */
    public void submitIou(Command command, String party) {
        String workflowId = randomUUID().toString();
        String applicationId = "IouApp";
        String commandId = randomUUID().toString();
        // Difference between following two - Min: PT11S, Max: PT2M
        Instant ledgerEffectiveTime = now();
        Instant maximumRecordTime = now().plusSeconds(11);
        List<Command> commands = singletonList(command);

        client.getCommandClient().submitAndWait(workflowId, applicationId, commandId, party, ledgerEffectiveTime, maximumRecordTime, commands).blockingGet();
        txCount.increment();
    }

    /**
     * Get ledger host IP.
     */
    public String getLedgerHost() {
        return ledgerHost;
    }

    /**
     * Get number of transactions done.
     */
    public int getTxCount() {
        return txCount.intValue();
    }

    @Override
    public String toString() {
        return getLedgerHost();
    }

}

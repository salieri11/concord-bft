package dappbench;

import static com.daml.ledger.rxjava.DamlLedgerClient.forHostWithLedgerIdDiscovery;
import static java.lang.Long.parseLong;
import static java.util.Optional.empty;
import static org.apache.logging.log4j.LogManager.getLogger;

import java.util.concurrent.atomic.AtomicInteger;

import org.apache.logging.log4j.Logger;

import com.daml.ledger.javaapi.data.Command;
import com.daml.ledger.javaapi.data.LedgerOffset;
import com.daml.ledger.javaapi.data.LedgerOffset.Absolute;
import com.daml.ledger.rxjava.DamlLedgerClient;
import com.digitalasset.quickstart.iou.IouMain;

/**
 * Client for a DAML ledger API. It uses the code from IOU quick start DApp.
 * 
 * {@link https://docs.daml.com/getting-started/quickstart.html#download-the-quickstart-application}
 */
public class DamlClient {

    private final static Logger logger = getLogger(DamlClient.class);

    private final String ledgerHost;
    private final int ledgerPort;

    private DamlLedgerClient client;
    private AtomicInteger txCount;

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
        txCount = new AtomicInteger();
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
        IouMain.submit(client, party, command);
        txCount.incrementAndGet();
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
        return txCount.get();
    }

    @Override
    public String toString() {
        return getLedgerHost();
    }

}

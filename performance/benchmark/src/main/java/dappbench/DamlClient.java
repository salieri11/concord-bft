package dappbench;

import com.daml.ledger.javaapi.data.Command;
import com.daml.ledger.javaapi.data.LedgerOffset;
import com.daml.ledger.javaapi.data.LedgerOffset.Absolute;
import com.daml.ledger.rxjava.DamlLedgerClient;
import com.digitalasset.quickstart.model.iou.Iou;
import org.apache.logging.log4j.Logger;

import java.math.BigDecimal;
import java.time.Instant;
import java.util.List;
import java.util.concurrent.ThreadLocalRandom;

import static java.lang.Long.parseLong;
import static java.time.Instant.now;
import static java.util.Collections.emptyList;
import static java.util.UUID.randomUUID;
import static java.util.stream.Collectors.toList;
import static java.util.stream.Stream.generate;
import static org.apache.logging.log4j.LogManager.getLogger;

/**
 * Client for a DAML ledger API. It uses the code from IOU quick start DApp.
 *
 * @see <a href="https://docs.daml.com/getting-started/quickstart.html#download-the-quickstart-application</a>
 */
public class DamlClient extends WorkloadClient {

    private static final Logger logger = getLogger(DamlClient.class);

    private final String party;
    private final int noOfCommandsPerTransaction;
    private final boolean logging;

    private DamlLedgerClient client;

    /**
     * Constructor.
     */
    public DamlClient(String ledgerHost, int ledgerPort, String party, int noOfCommandsPerTransaction, boolean logging) {
        super(ledgerHost, ledgerPort);
        this.party = party;
        this.noOfCommandsPerTransaction = noOfCommandsPerTransaction;
        this.logging = logging;
    }

    /**
     * Connects to the ledger and runs initial validation.
     */
    public void init() {
        client = DamlLedgerClient.newBuilder(getHost(), getPort()).build();
        client.connect();
        String ledgerId = client.getLedgerId();
        logger.debug("ledger-id for node {} is {}", getHost(), ledgerId);
    }

    /**
     * Get offset at end of the ledger.
     */
    @Override
    protected long getLedgerSize() {
        LedgerOffset offset = client.getTransactionsClient().getLedgerEnd().blockingGet();
        return parseLong(((Absolute) offset).getOffset());
    }

    @Override
    protected void doExecute() {
        List<Command> commands = generate(this::createCommand).limit(noOfCommandsPerTransaction).collect(toList());
        submitIou(commands, party);
    }

    /**
     * Create a contract to submit
     */
    private Command createCommand() {
        int iouAmount = ThreadLocalRandom.current().nextInt(10_000) + 1;
        Iou iou = new Iou("Alice", "Alice", "AliceCoin", new BigDecimal(iouAmount), emptyList());
        if (logging) {
            logger.info("{}", iou);
        }
        return iou.create();
    }

    /**
     * Submit IOU creation.
     */
    private void submitIou(List<Command> commands, String party) {
        String workflowId = randomUUID().toString();
        String applicationId = "IouApp";
        String commandId = randomUUID().toString();
        // Difference between following two - Min: PT11S, Max: PT2M
        Instant ledgerEffectiveTime = now();
        Instant maximumRecordTime = now().plusSeconds(11);

        client.getCommandClient().submitAndWait(workflowId, applicationId, commandId, party, ledgerEffectiveTime, maximumRecordTime, commands).blockingGet();
    }


}

package tee.tr;

import java.time.Instant;

/**
 * Update events for data and hashes.
 */
public interface TrEventHandler {

    /**
     * Callback when thin replica client subscriber receives data from a node.
     */
    void onDataReceived(long blockId, long updateTimeMillis);

    /**
     * Callback when thin replica client subscriber receives hash from a node.
     */
    void onHashReceived(long blockId);
}

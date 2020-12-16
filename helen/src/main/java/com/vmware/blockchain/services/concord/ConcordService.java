/*
 * Copyright (c) 2019 VMware, Inc. All rights reserved. VMware Confidential
 */

package com.vmware.blockchain.services.concord;

import java.util.List;
import java.util.UUID;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import com.vmware.blockchain.common.ConcordConnectionException;
import com.vmware.blockchain.common.ErrorCodeType;
import com.vmware.blockchain.connections.ConcordConnectionPool;
import com.vmware.blockchain.connections.ConnectionPoolManager;
import com.vmware.concord.Concord;
import com.vmware.concord.Concord.ConcordRequest;
import com.vmware.concord.Concord.ConcordResponse;
import com.vmware.concord.Concord.Peer;
import com.vmware.concord.Concord.PeerRequest;
import com.vmware.concord.Concord.PeerResponse;
import com.vmware.concord.ConcordHelper;
import com.vmware.concord.IConcordConnection;

/**
 * Some calls that go directly to concord, bypassing any compiler frontend.
 */
@Service
public class ConcordService {
    private ConnectionPoolManager connectionPoolManager;

    @Autowired
    public ConcordService(ConnectionPoolManager connectionPoolManager) {
        this.connectionPoolManager = connectionPoolManager;
    }

    private Concord.ConcordResponse forwardToConcord(UUID bid, ConcordRequest req)
            throws ConcordConnectionException {
        IConcordConnection conn = null;
        ConcordResponse concordResponse;
        ConcordConnectionPool concordConnectionPool = connectionPoolManager.getPool(bid);
        try {
            conn = concordConnectionPool.getConnection();
            if (conn == null) {
                throw new ConcordConnectionException(ErrorCodeType.CONCORD_CONNECTION);
            }
            boolean res = ConcordHelper.sendToConcord(req, conn);
            if (!res) {
                throw new ConcordConnectionException(ErrorCodeType.CONCORD_SEND_FAILED);
            }

            // receive response from Concord
            concordResponse = ConcordHelper.receiveFromConcord(conn);
            if (concordResponse == null) {
                throw new ConcordConnectionException(ErrorCodeType.CONCORD_INVALID_RESPONSE);
            }
            return concordResponse;
        } catch (Exception e) {
            throw new ConcordConnectionException(ErrorCodeType.CONCORD_INTERNAL_ERROR + e.getMessage());
        } finally {
            concordConnectionPool.putConnection(conn);
        }
    }

    /**
     * Query Concord to get the members of this cluster.
     * @param bid Blockchain ID for cluster
     */
    public List<Peer> getMembers(UUID bid) {
        // Construct a peer request object. Set its return_peers field.
        final PeerRequest peerRequestObj = PeerRequest.newBuilder().setReturnPeers(true).build();

        // Envelope the peer request object into an concord object.
        final ConcordRequest concordrequestObj =
                ConcordRequest.newBuilder().setPeerRequest(peerRequestObj).build();
        // Extract the peer response from the concord reponse envelope.
        ConcordResponse concordResponse = forwardToConcord(bid, concordrequestObj);
        PeerResponse peerResponse = concordResponse.getPeerResponse();

        // Read list of peer objects from the peer response object.
        return peerResponse.getPeerList();
    }

}

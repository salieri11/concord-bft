/*
 * Copyright (c) 2018-2019 VMware, Inc. All rights reserved. VMware Confidential
 */

package com.vmware.blockchain.connections;

import java.io.IOException;

import com.vmware.concord.Concord;
import com.vmware.concord.IConcordConnection;

import lombok.AllArgsConstructor;
import lombok.Getter;

/**
 * Mock Concord connection, used for testing.
 */
@Getter
@AllArgsConstructor
public class MockConnection implements IConcordConnection {
    // Pre-allocated check response
    private static Concord.ProtocolResponse protocolResponse =
        Concord.ProtocolResponse.newBuilder().setServerVersion(1).build();
    private static Concord.ConcordResponse concordResponse =
        Concord.ConcordResponse.newBuilder().setProtocolResponse(protocolResponse).build();

    String host;
    int ip;

    public String getIpStr() {
        return String.format("%s:%s", host, ip);
    }

    @Override
    public void close() {

    }

    @Override
    public void send(byte[] msg) throws IOException {
        // MockConnection just throws the request away
    }

    @Override
    /**
     * this method should be extended to remember last message sent and to return corresponding response. Currently it
     * returns hardcoded ConcordProtocolResponse message
     */
    public byte[] receive() throws IOException {
        return concordResponse.toByteArray();
    }

    @Override
    public boolean check() {
        return true;
    }

}

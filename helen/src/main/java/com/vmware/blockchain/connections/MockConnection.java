/*
 * Copyright (c) 2018 VMware, Inc. All rights reserved. VMware Confidential
 */

package com.vmware.blockchain.connections;

import java.nio.ByteBuffer;
import java.nio.ByteOrder;

import com.vmware.blockchain.common.ConcordProperties;
import com.vmware.concord.Concord;

/**
 * Mock Concord connection, used for testing.
 */
public class MockConnection implements IConcordConnection {
    private static Concord.ProtocolResponse protocolResponse =
            Concord.ProtocolResponse.newBuilder().setServerVersion(1).build();
    private ConcordProperties config;

    public MockConnection(ConcordProperties config) {
        this.config = config;
    }

    @Override
    public void close() {

    }

    @Override
    public boolean send(byte[] msg) {
        return true;
    }

    @Override
    /**
     * this method should be extended to remember last message sent and to return corresponding response. Currently it
     * returns hardcoded ConcordProtocolResponse message
     */
    public byte[] receive() {
        byte[] data = protocolResponse.toByteArray();
        int headerLength = config.getReceiveHeaderSizeBytes();
        byte[] bytes = ByteBuffer.allocate(headerLength + data.length).order(ByteOrder.LITTLE_ENDIAN)
                .putShort((short) data.length).put(data).array();
        return bytes;
    }

    @Override
    public boolean check() {
        return true;
    }

}

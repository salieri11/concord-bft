/*
 * Copyright (c) 2018 VMware, Inc. All rights reserved. VMware Confidential
 */

package com.vmware.blockchain.connections;

import java.nio.ByteBuffer;
import java.nio.ByteOrder;

import com.vmware.athena.Athena;
import com.vmware.blockchain.common.AthenaProperties;

public class MockConnection implements IAthenaConnection {
    private static Athena.ProtocolResponse protocolResponse =
            Athena.ProtocolResponse.newBuilder().setServerVersion(1).build();
    private AthenaProperties config;

    public MockConnection(AthenaProperties config) {
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
     * returns hardcoded AthenaProtocolResponse message
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

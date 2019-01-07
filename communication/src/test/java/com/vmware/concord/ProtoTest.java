/*
 * Copyright (c) 2018-2019 VMware, Inc. All rights reserved. VMware Confidential
 */

package com.vmware.concord;

import com.google.protobuf.InvalidProtocolBufferException;

import com.vmware.concord.Concord.ConcordRequest;
import com.vmware.concord.Concord.ProtocolRequest;

import junit.framework.Test;
import junit.framework.TestCase;
import junit.framework.TestSuite;

/**
 * This is mostly a sanity test, to make sure that protobuf compilation happens correctly.
 */
public class ProtoTest  extends TestCase {
    /**
     * Create the test case.
     *
     * @param testName name of the test case
     */
    public ProtoTest(String testName) {
        super(testName);
    }

    /**
     * Get the suite.
     *
     * @return the suite of tests being tested
     */
    public static Test suite() {
        return new TestSuite(ProtoTest.class);
    }

    /**
     * Create a very simple message. Encode it, decode it, and check that the result matches. This is not a rigorous
     * test, but will hopefully at least prove that we compiled the .proto correctly.
     */
    public void testRoundtripEncoding() throws InvalidProtocolBufferException {
        // our expected value
        final int clientVersion = 42;

        // Construct a message
        ProtocolRequest.Builder protoReqB = ProtocolRequest.newBuilder();
        protoReqB.setClientVersion(clientVersion);
        ConcordRequest.Builder concReqB = ConcordRequest.newBuilder();
        concReqB.setProtocolRequest(protoReqB);

        // Roundtrip encode/decode
        byte[] encoded = concReqB.build().toByteArray();
        ConcordRequest concReq = ConcordRequest.parseFrom(encoded);

        // Check for a match
        assertTrue(concReq.hasProtocolRequest());
        assertTrue(concReq.getProtocolRequest().hasClientVersion());
        assertEquals(clientVersion, concReq.getProtocolRequest().getClientVersion());
    }
}

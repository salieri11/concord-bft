/*
 * Copyright (c) 2018-2019 VMware, Inc. All rights reserved. VMware Confidential
 */

package com.vmware.concord;

import java.io.IOException;
import java.util.Random;

import org.junit.After;
import org.junit.Assert;
import org.junit.Before;
import org.junit.Test;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.google.protobuf.ByteString;

/**
 * Basic tests of ConcordTcpConnection. This uses a mock server, so that we can deal with just our client
 * side. Integration with the server side is tested by the Hermes integration tests.
 */
public class TcpTest {
    private static Logger logger = LoggerFactory.getLogger(TcpTest.class);

    private MockTcpServer server;

    // This is a local echo server, it should be fast.
    private static final int receiveTimeoutMs = 1000;

    /**
     * Start and wait for the mock server.
     */
    @Before
    public void startServer() {
        server = new MockTcpServer();
        if (!server.startServer()) {
            Exception e = server.getLastException();
            e.printStackTrace();
            Assert.fail("Mock server did not start: " + e);
        }
    }

    /**
     * Tear down the mock server.
     */
    @After
    public void stopServer() {
        if (server != null) {
            server.stopServer();
            server = null;
        }
    }

    /**
     * This is the very basic, expected configuration, happy path initial protocol setup. It opens a connection to the
     * mock, sends the initial protocol-check message, and verifies both that the client decoded a successful response,
     * and that the mock server actually received the request. If this test breaks, it means either something simple
     * broke, or some default configuration changed, like the protocol version.
     */
    @Test
    public void testConnectionCheckSuccess() throws IOException {
        // Open connection, and make sure the client side believes it checks out.
        ConcordTcpConnection conn = new ConcordTcpConnection(receiveTimeoutMs, server.getHeaderSizeBytes(),
                                                             server.getHostName(), server.getPort());
        Assert.assertEquals(true, conn.check());

        // Then make sure that the client side isn't blindly returning true, and that the server did actually receive
        // the check request, and sent a check response. The client could still be lying, but we have some indication
        // that it didn't have to be.
        Concord.ConcordRequest req = server.getLastRequest();
        Assert.assertNotNull(req);
        Assert.assertTrue(req.hasProtocolRequest());

        Concord.ConcordResponse resp = server.getLastResponse();
        Assert.assertNotNull(resp);
        Assert.assertTrue(resp.hasProtocolResponse());
    }

    /**
     * Test that the client handles rejection from the server correctly. This is the other half of the check for lies
     * from above. The server is going to respond that the client version is not supported, so the client should report
     * that the connection did not check out.
     */
    @Test
    public void testConnectionCheckRejection() throws IOException {
        server.setRejectClientVersion();

        // Open connection, and make sure the client side believes it does not check out.
        ConcordTcpConnection conn = new ConcordTcpConnection(receiveTimeoutMs, server.getHeaderSizeBytes(),
                                                             server.getHostName(), server.getPort());
        Assert.assertEquals(false, conn.check());
    }

    /**
     * Test actual message data, and thus ConcordHelper. Use the echo feature of TestRequest to send random data.
     */
    @Test
    public void testMessageData() throws IOException {
        ConcordTcpConnection conn = new ConcordTcpConnection(receiveTimeoutMs, server.getHeaderSizeBytes(),
                                                             server.getHostName(), server.getPort());
        Assert.assertEquals(true, conn.check());

        // Get some data to echo. Randomness is just a small hurdle to fight "coding to the test".
        byte[] echoString = new byte[64];
        new Random().nextBytes(echoString);
        final ByteString echoByteString = ByteString.copyFrom(echoString);

        final Concord.ConcordRequest request = Concord.ConcordRequest.newBuilder()
            .setTestRequest(Concord.TestRequest.newBuilder().setEchoBytes(echoByteString))
            .build();

        // Send and receive the data. Make sure it's the same data.
        ConcordHelper.sendToConcord(request, conn);
        final Concord.ConcordResponse response = ConcordHelper.receiveFromConcord(conn);
        Assert.assertNotNull(response);
        Assert.assertTrue(response.hasTestResponse());
        Assert.assertTrue(response.getTestResponse().hasEcho());
        Assert.assertEquals(echoByteString, response.getTestResponse().getEchoBytes());

        // Make sure the server really saw our request, and that it really sent our response.
        final Concord.ConcordRequest serverRequest = server.getLastRequest();
        Assert.assertEquals(request, serverRequest);
        final Concord.ConcordResponse serverResponse = server.getLastResponse();
        Assert.assertEquals(response, serverResponse);
    }

    /**
     * Test actual error message, and thus ConcordHelper. The rejection error message was hidden from our test in
     * testConnectionCheckRejection. Send an invalid EthRequest to make an error happen.
     */
    @Test
    public void testErrorMessage() throws IOException {
        ConcordTcpConnection conn = new ConcordTcpConnection(receiveTimeoutMs, server.getHeaderSizeBytes(),
                                                             server.getHostName(), server.getPort());
        Assert.assertEquals(true, conn.check());

        // An EthRequest with no method is invalid.
        final Concord.ConcordRequest request = Concord.ConcordRequest.newBuilder()
            .addEthRequest(Concord.EthRequest.newBuilder())
            .build();

        // Send and receive the data. Make sure it's an error.
        ConcordHelper.sendToConcord(request, conn);
        final Concord.ConcordResponse response = ConcordHelper.receiveFromConcord(conn);
        Assert.assertNotNull(response);
        Assert.assertEquals(0, response.getEthResponseCount());
        Assert.assertEquals(1, response.getErrorResponseCount());
        Assert.assertTrue(response.getErrorResponse(0).hasDescription());
        Assert.assertEquals(server.getEthErrorResponseString(), response.getErrorResponse(0).getDescription());

        // Make sure the server really saw our request, and that it really sent our response.
        final Concord.ConcordRequest serverRequest = server.getLastRequest();
        Assert.assertEquals(request, serverRequest);
        final Concord.ConcordResponse serverResponse = server.getLastResponse();
        Assert.assertEquals(response, serverResponse);
    }

    /**
     * Concord doesn't like empty messages. It closes connections, and this makes Helen unhappy sometimes. Checks have
     * been put in the client side to prevent empty messages from going to Concord. This test makes sure those checks
     * are in place.
     */
    @Test
    public void testEmptyMessage() throws IOException {
        ConcordTcpConnection conn = new ConcordTcpConnection(receiveTimeoutMs, server.getHeaderSizeBytes(),
                                                             server.getHostName(), server.getPort());
        Assert.assertEquals(true, conn.check());

        // An empty request
        final Concord.ConcordRequest request = Concord.ConcordRequest.newBuilder().build();

        try {
            ConcordHelper.sendToConcord(request, conn);
            Assert.fail("Sending an empty message should have thrown an exception.");
        } catch (IOException e) {
            // This is the expected path. The message may change - please to change it to match ConcordTcpConnection.
            // It is checked here just to make sure that we didn't get some other IOException.
            Assert.assertEquals("Empty request", e.getMessage());
        }
    }
}

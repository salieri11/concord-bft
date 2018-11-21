/*
 * Copyright (c) 2018 VMware, Inc. All rights reserved. VMware Confidential
 */

package com.vmware.blockchain.services.ethereum;

import java.io.IOException;
import java.nio.ByteBuffer;
import java.nio.ByteOrder;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

import com.google.protobuf.InvalidProtocolBufferException;
import com.vmware.blockchain.common.ConcordProperties;
import com.vmware.blockchain.connections.IConcordConnection;
import com.vmware.concord.Concord;

/**
 * Some helper functions dealing with protobuf.
 */
public class ConcordHelper {

    private static Logger log = LogManager.getLogger(ConcordHelper.class);

    /**
     * Converts an int into two bytes.
     *
     * @param value that needs to be converted
     * @param size size of returned byte array
     * @return A byte array containing two bytes.
     */
    private static byte[] intToSizeBytes(int value, int size) {
        byte[] bytes = ByteBuffer.allocate(size).order(ByteOrder.LITTLE_ENDIAN).putShort((short) value).array();
        return bytes;
    }

    /**
     * Sends a Google Protocol Buffer request to Concord. Concord expects two bytes signifying the size of the request
     * before the actual request.
     */
    public static boolean sendToConcord(Concord.ConcordRequest request, IConcordConnection conn, ConcordProperties conf)
            throws IOException {
        // here specifically, request.toString() it time consuming,
        // so checking level enabled can gain performance
        if (log.isTraceEnabled()) {
            log.trace(String.format("Sending request to Concord : %s %s", System.lineSeparator(), request));
        }

        // Find size of request and pack size into two bytes.
        int requestSize = request.getSerializedSize();

        // If the request size doesn't fit in two bytes, abort.
        if (requestSize > 65535) {
            throw new IOException("Request too large: " + requestSize);
        }

        byte[] size = intToSizeBytes(requestSize, conf.getReceiveHeaderSizeBytes());
        byte[] protobufRequest = request.toByteArray();
        ByteBuffer msg = ByteBuffer.allocate(size.length + protobufRequest.length);
        msg.put(size, 0, size.length);
        msg.put(protobufRequest, 0, protobufRequest.length);

        // Write requests over the output stream.
        boolean res = conn.send(msg.array());
        return res;
    }

    /**
     * Receives a Google Protocol Buffer response from Concord. Concord sends two bytes signifying the size of the
     * response before the actual response.
     **/
    public static Concord.ConcordResponse receiveFromConcord(IConcordConnection conn) {
        try {
            byte[] data = conn.receive();
            if (data == null) {
                return null;
            }

            // Convert read bytes into a Protocol Buffer object.
            Concord.ConcordResponse concordResponse;
            try {
                concordResponse = Concord.ConcordResponse.parseFrom(data);
            } catch (InvalidProtocolBufferException e) {
                log.error("Error in parsing Concord's response");
                throw new InvalidProtocolBufferException(e.getMessage());
            }

            return concordResponse;
        } catch (Exception e) {
            log.error("receiveFromConcord", e);
            return null;
        }
    }
}

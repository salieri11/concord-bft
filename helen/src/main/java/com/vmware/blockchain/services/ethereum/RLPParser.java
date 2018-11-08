/*
 * Copyright (c) 2018 VMware, Inc. All rights reserved. VMware Confidential
 */

package com.vmware.blockchain.services.ethereum;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

import com.google.protobuf.ByteString;

class RLPParser {

    Logger logger = LogManager.getLogger(RLPParser.class);

    private ByteString input;
    private int offset = 0;

    public RLPParser(String hexData) throws APIHelper.HexParseException {
        input = APIHelper.hexStringToBinary(hexData);
    }

    public RLPParser(ByteString binData) {
        input = binData;
    }

    public boolean atEnd() {
        return offset == input.size();
    }

    public ByteString next() throws RLPEmptyException {
        if (atEnd()) {
            throw new RLPEmptyException();
        }

        int tag = 0xff & input.byteAt(offset);
        offset++;
        if (tag < 0x80) {
            logger.debug("Tag: " + String.format("0x%02X", tag) + " == self byte value");
            return input.substring(offset - 1, offset);
        } else if (tag < 0xb8) {
            int length = tag - 0x80;
            logger.debug("Tag: " + String.format("0x%02X", tag) + " == short string, length=" + length);
            return shortRun(length);
        } else if (tag < 0xc0) {
            int lengthLength = tag - 0xb7;
            logger.debug("Tag: " + String.format("0x%02X", tag) + " == long string lengthLength=" + lengthLength);
            return longRun(lengthLength);
        } else if (tag < 0xf8) {
            int length = tag - 0xc0;
            logger.debug("Tag: " + String.format("0x%02X", tag) + " == short list, length=" + length);
            return shortRun(length);
        } else {
            int lengthLength = tag - 0xf7;
            logger.debug("Tag: " + String.format("0x%02X", tag) + " == long list lengthLength=" + lengthLength);
            return longRun(lengthLength);
        }
    }

    private ByteString shortRun(int length) throws RLPEmptyException {
        if (offset + length > input.size()) {
            offset = input.size();
            throw new RLPEmptyException();
        }

        ByteString value = input.substring(offset, offset + length);
        offset += length;
        return value;
    }

    private ByteString longRun(int lengthLength) throws RLPEmptyException {
        if (offset + lengthLength > input.size()) {
            offset = input.size();
            throw new RLPEmptyException();
        }

        /*
         * ByteString only supports 32-bit integer length. If the RLP specifies something larger than that, it's
         * probably lying, or Helen is probably already crashing if it got this far. Skip the length bytes above the
         * 4-byte limit, and then go on to read the length they say to read. If we continue after this, the request will
         * be rejected when EthSendTxHandler notices we didn't consume the whole RLP message.
         */
        if (lengthLength > 4) {
            offset += lengthLength - 4;
            lengthLength = 4;
        }

        int length = 0;
        for (; lengthLength > 0; lengthLength--) {
            length = (length << 8) | (0xff & input.byteAt(offset));
            offset++;
        }

        if (offset + length > input.size()) {
            offset = input.size();
            throw new RLPEmptyException();
        }

        ByteString value = input.substring(offset, offset + length);
        offset += length;
        return value;
    }

    public static class RLPEmptyException extends Exception {
        public RLPEmptyException() {
            super("No more bytes in RLP string");
        }
    }
}

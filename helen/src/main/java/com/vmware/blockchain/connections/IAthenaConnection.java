/*
 * Copyright (c) 2018 VMware, Inc. All rights reserved. VMware Confidential
 */

package com.vmware.blockchain.connections;

/**
 * Athena Connection interface.
 */
public interface IAthenaConnection {
    void close();

    boolean send(byte[] msg);

    byte[] receive();

    boolean check();
}

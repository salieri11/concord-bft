/*
 * Copyright (c) 2018 VMware, Inc. All rights reserved. VMware Confidential
 */

package com.vmware.blockchain.connections;

/**
 * Concord Connection interface.
 */
public interface IConcordConnection {
    void close();

    boolean send(byte[] msg);

    byte[] receive();

    boolean check();
}

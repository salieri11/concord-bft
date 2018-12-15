/*
 * Copyright (c) 2018 VMware, Inc. All rights reserved. VMware Confidential
 */

package com.vmware.concord;

import java.io.IOException;

/**
 * Concord Connection interface.
 */
public interface IConcordConnection {
    void close();

    void send(byte[] msg) throws IOException;

    byte[] receive() throws IOException;

    boolean check();
}

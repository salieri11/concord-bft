/*
 * Copyright (c) 2019 VMware, Inc. All rights reserved. VMware Confidential
 */

package com.vmware.blockchain.services.contracts;

/**
 * JPA projection to get version info.
 */
public interface VersionProjection {
    String getVersionName();

    String getMetadata();

    String getAddress();

    String getOwner();
}

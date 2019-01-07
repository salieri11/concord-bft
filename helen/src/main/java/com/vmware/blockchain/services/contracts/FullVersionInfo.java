/*
 * Copyright (c) 2018-2019 VMware, Inc. All rights reserved. VMware Confidential
 */

package com.vmware.blockchain.services.contracts;

/**
 * An interface for retrieving complete information about a particular contract.
 */
public interface FullVersionInfo extends BriefVersionInfo {
    String getByteCode();

    String getSourceCode();
}

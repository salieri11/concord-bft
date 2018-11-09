/*
 * Copyright (c) 2018 VMware, Inc. All rights reserved. VMware Confidential
 */

package com.vmware.blockchain.services.contracts;

/**
 * An interface for retrieving brief information about a particular version of a particular contract.
 */
public interface BriefVersionInfo extends BriefContractInfo {
    String getAddress();

    String getMetaData();

    String getVersionName();
}

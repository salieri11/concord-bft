/*
 * Copyright (c) 2020 VMware, Inc. All rights reserved. VMware Confidential
 */

package com.vmware.blockchain.configuration.util;

import lombok.Builder;
import lombok.Getter;

/**
 * Holder class for Blockchain features.
 */
@Builder
@Getter
public class BlockchainFeatures {
    private boolean isObjectStoreEnabled;
    private boolean isSplitConfig;
    private boolean isPreExecutionDeployment;
    private int preExecutionThreshold;
    private boolean isTrcTlsEnabled;
}
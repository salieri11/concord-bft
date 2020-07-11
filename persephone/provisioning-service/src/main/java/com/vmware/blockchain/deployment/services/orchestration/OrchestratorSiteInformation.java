/*
 * Copyright (c) 2018-2019 VMware, Inc. All rights reserved. VMware Confidential
 */

package com.vmware.blockchain.deployment.services.orchestration;

import lombok.Data;

/**
 * Holds all the data classes.
 */
@Data
public class OrchestratorSiteInformation {

    String resourcePool;
    String dataStore;
    String network;
    String folder;
}
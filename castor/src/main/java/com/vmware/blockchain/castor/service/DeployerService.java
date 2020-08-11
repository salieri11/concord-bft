/*
 * Copyright (c) 2020 VMware, Inc. All rights reserved. VMware Confidential
 */

package com.vmware.blockchain.castor.service;

/**
 * Deployer service.
 */
public interface DeployerService {
    static final String OUTPUT_DIR_LOC_KEY = "castor.output.directory.location";
    static final String DEPL_DESC_LOC_KEY = "castor.deployment.descriptor.location";
    static final String INFRA_DESC_LOC_KEY = "castor.infrastructure.descriptor.location";

    void start();
}

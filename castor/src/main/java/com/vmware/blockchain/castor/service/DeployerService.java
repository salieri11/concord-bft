/*
 * Copyright (c) 2020 VMware, Inc. All rights reserved. VMware Confidential
 */

package com.vmware.blockchain.castor.service;

/**
 * Deployer service.
 */
public interface DeployerService {
    static final String DEPLOYMENT_TYPE_KEY = "castor_deployment_type";
    static final String OUTPUT_DIR_LOC_KEY = "castor_output_directory_location";
    static final String INFRA_DESC_LOC_KEY = "castor_infrastructure_descriptor_location";
    static final String DEPL_DESC_LOC_KEY = "castor_deployment_descriptor_location";
    void start();
}

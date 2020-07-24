/*
 * Copyright (c) 2020 VMware, Inc. All rights reserved. VMware Confidential
 */

package com.vmware.blockchain.castor.service;

import com.vmware.blockchain.castor.model.DeploymentDescriptorModel;
import com.vmware.blockchain.castor.model.InfrastructureDescriptorModel;

/**
 * Descriptor service to process descriptor files and models.
 */
public interface DescriptorService {
    DeploymentDescriptorModel readDeploymentDescriptorSpec(String deploymentDescriptorLocation);

    InfrastructureDescriptorModel readInfrastructureDescriptorSpec(String infrastructureDescriptorLocation);
}

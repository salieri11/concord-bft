/*
 * Copyright (c) 2020 VMware, Inc. All rights reserved. VMware Confidential
 */

package com.vmware.blockchain.castor.service;

import java.util.List;

import com.vmware.blockchain.castor.model.DeploymentDescriptorModel;
import com.vmware.blockchain.castor.model.InfrastructureDescriptorModel;

/**
 * Validator Service definition.
 */
public interface ValidatorService {
    /**
     * Validate the contents of the infrastructure and deployment descriptor files.
     * @param infra      The infrastructure descriptor model to be validated
     * @param deployment The deployment descriptor model to be validated
     * @return A list of validation errors
     */
    List<ValidationError> validate(InfrastructureDescriptorModel infra, DeploymentDescriptorModel deployment);
}

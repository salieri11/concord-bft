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
    static final String DEPLOYMENT_NUM_CLIENTS_RANGE_KEY = "castor.deployment.num.clients.range";
    static final String DEPLOYMENT_MAX_CLIENTS_PER_GROUP = "castor.deployment.max.clients.per.group";
    static final String DEPLOYMENT_MAX_CLIENT_GROUPS = "castor.deployment.max.client.groups";

    /**
     * Validate the contents of the infrastructure and deployment descriptor files.
     *
     * @param deploymentType the type of deployment that determines the validator to use
     * @param infra      The infrastructure descriptor model to be validated
     * @param deployment The deployment descriptor model to be validated
     * @return A list of validation errors
     */
    List<ValidationError> validate(CastorDeploymentType deploymentType,
                                   InfrastructureDescriptorModel infra, DeploymentDescriptorModel deployment);
}

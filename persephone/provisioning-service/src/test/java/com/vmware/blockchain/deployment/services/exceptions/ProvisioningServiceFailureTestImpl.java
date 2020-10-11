/*
 * Copyright (c) 2020 VMware, Inc. All rights reserved. VMware Confidential
 */

package com.vmware.blockchain.deployment.services.exceptions;

import com.vmware.blockchain.deployment.services.exception.BadRequestPersephoneException;
import com.vmware.blockchain.deployment.services.exception.NotFoundPersephoneException;
import com.vmware.blockchain.deployment.v1.DeploymentRequest;
import com.vmware.blockchain.deployment.v1.DeploymentRequestResponse;
import com.vmware.blockchain.deployment.v1.DeprovisionDeploymentRequest;
import com.vmware.blockchain.deployment.v1.DeprovisionDeploymentResponse;
import com.vmware.blockchain.deployment.v1.ProvisioningServiceV2Grpc;

import io.grpc.stub.StreamObserver;

/**
 * Implement test methods for Provisioning service's skeleton.
 */
public class ProvisioningServiceFailureTestImpl extends ProvisioningServiceV2Grpc.ProvisioningServiceV2ImplBase {

    public static final String ERR_MSG_BAD_REQ = "Test exception - bad request";
    public static final String ERR_MSG_NOT_FOUND = "Test exception - not found";

    @Override
    public void createDeployment(DeploymentRequest request,
                                 StreamObserver<DeploymentRequestResponse> responseObserver) {
        throw new BadRequestPersephoneException(ERR_MSG_BAD_REQ, new RuntimeException("Bad request"));
    }

    @Override
    public void deprovisionDeployment(DeprovisionDeploymentRequest request,
                                      StreamObserver<DeprovisionDeploymentResponse> responseObserver) {
        throw new NotFoundPersephoneException(ERR_MSG_NOT_FOUND, new RuntimeException("Not Found"));
    }
}

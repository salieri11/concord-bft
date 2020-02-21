/* **************************************************************************
 * Copyright (c) 2019 VMware, Inc. All rights reserved. VMware Confidential
 * **************************************************************************/

package com.vmware.blockchain.deployment.services.orchestrationsite;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.lognet.springboot.grpc.GRpcService;

import com.vmware.blockchain.deployment.orchestration.Orchestrator;
import com.vmware.blockchain.deployment.orchestration.OrchestratorProvider;
import com.vmware.blockchain.deployment.v1.OrchestrationSiteServiceGrpc;
import com.vmware.blockchain.deployment.v1.ValidateOrchestrationSiteRequest;
import com.vmware.blockchain.deployment.v1.ValidateOrchestrationSiteResponse;

import io.grpc.stub.StreamObserver;

/**
 * Site service.
 */
@GRpcService
public class OrchestrationSiteService extends OrchestrationSiteServiceGrpc.OrchestrationSiteServiceImplBase {

    private static final Logger log = LogManager.getLogger(OrchestrationSiteService.class);

    private final OrchestratorProvider orchestratorProvider;

    /**
     * Constructor.
     */
    public OrchestrationSiteService(OrchestratorProvider orchestratorProvider) {
        this.orchestratorProvider = orchestratorProvider;
    }

    @Override
    public void validateOrchestrationSite(ValidateOrchestrationSiteRequest request,
                                          StreamObserver<ValidateOrchestrationSiteResponse> responseObserver) {
        Orchestrator newOrchestrator = orchestratorProvider.newOrchestrator(request.getSite());
        try {
            newOrchestrator.validate();
            ValidateOrchestrationSiteResponse response = ValidateOrchestrationSiteResponse.newBuilder()
                    .setHeader(request.getHeader())
                    .setSite(request.getSite())
                    .build();

            log.info("Validation request(${request.header.id}), "
                     + "type(${request.site.type}, labels(${request.site.labels})");
            responseObserver.onNext(response);
        } catch (Exception e) {
            log.error("Error validating orchestration site, error: ", e);
            responseObserver.onError(e);
        } finally {
            responseObserver.onCompleted();
        }
    }
}
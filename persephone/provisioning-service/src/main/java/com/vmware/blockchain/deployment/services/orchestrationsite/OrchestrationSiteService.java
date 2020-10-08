/* **************************************************************************
 * Copyright (c) 2019 VMware, Inc. All rights reserved. VMware Confidential
 * **************************************************************************/

package com.vmware.blockchain.deployment.services.orchestrationsite;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.lognet.springboot.grpc.GRpcService;
import org.springframework.beans.factory.annotation.Autowired;

import com.vmware.blockchain.deployment.server.BootstrapComponent;
import com.vmware.blockchain.deployment.services.orchestration.Orchestrator;
import com.vmware.blockchain.deployment.services.orchestration.OrchestratorProvider;
import com.vmware.blockchain.deployment.services.orchestration.ipam.IpamClient;
import com.vmware.blockchain.deployment.services.orchestration.vmware.OrchestratorFactory;
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
    private IpamClient ipamClient;

    /**
     * Constructor.
     */
    @Autowired
    public OrchestrationSiteService(BootstrapComponent bootstrapComponent) {
        this.orchestratorProvider = new OrchestratorFactory();
        this.ipamClient = new IpamClient(bootstrapComponent.allocationService, bootstrapComponent.pathToCerts);
    }

    @Override
    public void validateOrchestrationSite(ValidateOrchestrationSiteRequest request,
                                          StreamObserver<ValidateOrchestrationSiteResponse> responseObserver) {
        Orchestrator newOrchestrator = orchestratorProvider.newOrchestrator(request.getSite(), ipamClient);
        try {
            newOrchestrator.populate();
            ValidateOrchestrationSiteResponse response = ValidateOrchestrationSiteResponse.newBuilder()
                    .setHeader(request.getHeader())
                    .setSite(request.getSite())
                    .build();


            // TODO cleanup session.

            log.info("Validation request: {}", request.getSite());
            responseObserver.onNext(response);
        } catch (Exception e) {
            log.error("Error validating orchestration site, error: ", e);
            responseObserver.onError(e);
        } finally {
            responseObserver.onCompleted();
        }
    }
}
/*
 * Copyright (c) 2020 VMware, Inc. All rights reserved. VMware Confidential
 */

package com.vmware.blockchain.castor.service;

import java.io.PrintWriter;
import java.util.List;
import java.util.concurrent.CompletableFuture;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.core.env.Environment;
import org.springframework.stereotype.Service;

import com.vmware.blockchain.castor.model.InfrastructureDescriptorModel;
import com.vmware.blockchain.castor.model.ProvisionDescriptorDescriptorModel;
import com.vmware.blockchain.deployment.v1.OrchestrationSiteInfo;
import com.vmware.blockchain.deployment.v1.OrchestrationSiteServiceGrpc;
import com.vmware.blockchain.deployment.v1.ValidateOrchestrationSiteRequest;

import lombok.extern.log4j.Log4j2;

/**
 * Site Validation service implementation.
 */
@Service
@Log4j2
public class SiteValidatorServiceImpl implements SiteValidatorService {

    private final Environment environment;
    private final OrchestrationSiteServiceGrpc.OrchestrationSiteServiceBlockingStub orchestrationSiteClient;

    @Autowired
    public SiteValidatorServiceImpl(
            Environment environment,
            OrchestrationSiteServiceGrpc.OrchestrationSiteServiceBlockingStub orchestrationSiteClient) {
        this.environment = environment;
        this.orchestrationSiteClient = orchestrationSiteClient;
    }

    @Override
    public void siteValidationHandoff(
            PrintWriter printWriter, InfrastructureDescriptorModel infrastructureDescriptorModel,
            ProvisionDescriptorDescriptorModel validationDeploymentDescriptorModel,
            CompletableFuture<CastorDeploymentStatus> deploymentCompletionFuture) {

        String consortiumName = validationDeploymentDescriptorModel.getBlockchain().getConsortiumName();
        printWriter.printf("Consortium Name: %s\n", consortiumName);

        // Loop through zones, and request a site validation for each zone. PS does not know anything about
        // the association between a zone name and a site info, hence this loop.
        boolean errors = false;
        List<InfrastructureDescriptorModel.Zone> zones = infrastructureDescriptorModel.getZones();
        for (InfrastructureDescriptorModel.Zone zone : zones) {
            String zoneName = zone.getName();
            log.info("Requesting validation for zone: {}", zoneName);
            OrchestrationSiteInfo orchestrationSiteInfo =
                    DeploymentHelper.toOrchestrationSiteInfo(zoneName, infrastructureDescriptorModel);
            ValidateOrchestrationSiteRequest validationRequest = ValidateOrchestrationSiteRequest.newBuilder()
                    .setSite(orchestrationSiteInfo)
                    .build();
            try {
                orchestrationSiteClient.validateOrchestrationSite(validationRequest);
                // Ignore the response from the above. If the call succeeds, the request has valid values.
                log.error("Site validation succeeded for zone: {}", zoneName);
                printWriter.printf("Site validation succeeded for zone: %s\n", zoneName);
            } catch (Exception e) {
                log.error("Site validation failed for zone: {} with exception: {}", zoneName, e);
                printWriter.printf("Site validation failed for zone: %s, error: %s\n", zoneName, e.getMessage());
                errors = true; // but continue validating other zones
            }
        }

        if (errors) {
            deploymentCompletionFuture.complete(CastorDeploymentStatus.FAILURE);
        }
        else {
            deploymentCompletionFuture.complete(CastorDeploymentStatus.SUCCESS);
        }
    }
}

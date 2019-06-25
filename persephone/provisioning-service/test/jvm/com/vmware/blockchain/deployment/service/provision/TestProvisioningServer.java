/* **************************************************************************
 * Copyright (c) 2019 VMware, Inc. All rights reserved. VMware Confidential
 * **************************************************************************/

package com.vmware.blockchain.deployment.service.provision;

import java.util.List;

import javax.inject.Singleton;

import com.vmware.blockchain.deployment.model.OrchestrationSite;

import dagger.BindsInstance;
import dagger.Component;

/**
 * Test interface for provisioning server.
 */
@Component(modules = {ProvisioningServiceModule.class, TestOrchestratorModule.class})
@Singleton
public interface TestProvisioningServer {
    ProvisioningService provisionService();

    /**
     * Builder class for testing.
     */
    @Component.Builder
    interface Builder {

        @BindsInstance
        Builder orchestrations(List<OrchestrationSite> entries);

        TestProvisioningServer build();
    }
}

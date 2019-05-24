/* **************************************************************************
 * Copyright (c) 2019 VMware, Inc. All rights reserved. VMware Confidential
 * **************************************************************************/

package com.vmware.blockchain.deployment.service.provision;

import java.util.Map;

import javax.inject.Singleton;

import com.vmware.blockchain.deployment.model.OrchestrationSiteIdentifier;
import com.vmware.blockchain.deployment.model.orchestration.OrchestrationSiteInfo;

import dagger.BindsInstance;
import dagger.Component;

/**
 * Test interface for provisioning server.
 */
@Component(modules = {ProvisionServiceModule.class, TestOrchestratorModule.class})
@Singleton
public interface TestProvisionServer {
    ProvisionService provisionService();

    /**
     * Builder class for testing.
     */
    @Component.Builder
    interface Builder {

        @BindsInstance
        Builder orchestrations(Map<OrchestrationSiteIdentifier, OrchestrationSiteInfo> entries);

        TestProvisionServer build();
    }
}

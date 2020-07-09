/*
 * Copyright (c) 2020 VMware, Inc. All rights reserved. VMware Confidential
 */

package com.vmware.blockchain.castor;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.context.annotation.Import;

import com.vmware.blockchain.castor.service.DescriptorService;
import com.vmware.blockchain.castor.service.DescriptorServiceImpl;
import com.vmware.blockchain.castor.service.ProvisionerService;
import com.vmware.blockchain.castor.service.ProvisionerServiceImpl;
import com.vmware.blockchain.deployment.v1.ProvisioningServiceV2Grpc;

/**
 * Spring configuration for test contexts.
 */
@Configuration
@Import(CastorConfiguration.class)
public class CastorTestConfiguration {

    @Autowired
    ProvisioningServiceV2Grpc.ProvisioningServiceV2BlockingStub blockingProvisioningClient;

    @Autowired
    ProvisioningServiceV2Grpc.ProvisioningServiceV2Stub asyncProvisioningClient;


    /**
     * Create a descriptor service bean.
     * @return the descriptor service singleton.
     */
    @Bean
    public DescriptorService descriptorService() {
        return new DescriptorServiceImpl();
    }

    /**
     * Create a provisioner service bean.
     * @return the provisioner service singleton.
     */
    @Bean
    public ProvisionerService provisionerService() {
        return new ProvisionerServiceImpl(blockingProvisioningClient, asyncProvisioningClient);
    }

}

/*
 * Copyright (c) 2020 VMware, Inc. All rights reserved. VMware Confidential
 */

package com.vmware.blockchain.castor;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.context.MessageSource;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.context.annotation.Import;
import org.springframework.core.env.Environment;
import org.springframework.mock.env.MockEnvironment;

import com.vmware.blockchain.castor.service.DeployerService;
import com.vmware.blockchain.castor.service.DeployerServiceImpl;
import com.vmware.blockchain.castor.service.DescriptorService;
import com.vmware.blockchain.castor.service.DescriptorServiceImpl;
import com.vmware.blockchain.castor.service.ProvisionerService;
import com.vmware.blockchain.castor.service.ProvisionerServiceImpl;
import com.vmware.blockchain.castor.service.ReconfigurerService;
import com.vmware.blockchain.castor.service.ReconfigurerServiceImpl;
import com.vmware.blockchain.castor.service.ValidatorService;
import com.vmware.blockchain.castor.service.ValidatorServiceImpl;
import com.vmware.blockchain.deployment.v1.ProvisioningServiceV2Grpc;

/**
 * Spring configuration for test contexts.
 */
@Configuration
@Import(CastorConfiguration.class)
public class CastorTestConfiguration {

    @Autowired
    private Environment environment;

    @Autowired
    private ProvisioningServiceV2Grpc.ProvisioningServiceV2BlockingStub blockingProvisioningClient;

    @Autowired
    private ProvisioningServiceV2Grpc.ProvisioningServiceV2Stub asyncProvisioningClient;

    @Autowired
    private MessageSource messageSource;

    /**
     * Create a mock environment for testing.
     * @return the Spring tests framework mock environment.
     */
    @Bean
    public MockEnvironment mockEnvironment() {
        return new MockEnvironment();
    }

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
        return new ProvisionerServiceImpl(environment, blockingProvisioningClient, asyncProvisioningClient);
    }

    /**
     * Create a reconfigurer service bean.
     * @return the reconfigurer service singleton.
     */
    @Bean
    public ReconfigurerService reconfigurerService() {
        return new ReconfigurerServiceImpl(blockingProvisioningClient);

    }

    /**
     * Create a validator service bean.
     * @return the validator service singleton.
     */
    @Bean
    public ValidatorService validatorService() {
        return new ValidatorServiceImpl(messageSource, environment);
    }

    /**
     * Create a deployer service bean.
     * @return the deployer service singleton.
     */
    @Bean
    public DeployerService deployerService() {
        return new DeployerServiceImpl(
                mockEnvironment(), descriptorService(), validatorService(),
                provisionerService(), reconfigurerService());
    }
}

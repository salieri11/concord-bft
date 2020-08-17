/*
 * Copyright (c) 2020 VMware, Inc. All rights reserved. VMware Confidential
 */

package com.vmware.blockchain.agent.utils;

import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;

import com.vmware.blockchain.deployment.v1.ConcordAgentConfiguration;

/**
 * Spring configuration for test contexts.
 */
@Configuration
public class AgentTestConfiguration {

    @Bean
    public ConcordAgentConfiguration concordAgentConfiguration() {
        return ConcordAgentConfiguration.newBuilder().build();
    }

}

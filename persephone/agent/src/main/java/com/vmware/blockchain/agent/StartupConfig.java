/*
 * Copyright (c) 2020 VMware, Inc. All rights reserved. VMware Confidential
 */

package com.vmware.blockchain.agent;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.context.event.ApplicationStartedEvent;
import org.springframework.context.annotation.Configuration;
import org.springframework.context.event.EventListener;

import com.vmware.blockchain.agent.services.NodeStartupOrchestrator;

import lombok.extern.slf4j.Slf4j;

/**
 * Handle the Startup event, and initialize things that need initializing.
 */
@Configuration
@Slf4j
public class StartupConfig {

    private NodeStartupOrchestrator nodeStartupOrchestrator;

    @Autowired
    public StartupConfig(NodeStartupOrchestrator nodeStartupOrchestrator) {
        this.nodeStartupOrchestrator = nodeStartupOrchestrator;
    }

    /**
     * Perform initialization tasks after Application is ready.
     */
    @EventListener(classes = ApplicationStartedEvent.class)
    public void applicationStarted() {
        log.info("Triggering bootstrap...");
        //nodeStartupOrchestrator.bootstrapConcord();
    }

}

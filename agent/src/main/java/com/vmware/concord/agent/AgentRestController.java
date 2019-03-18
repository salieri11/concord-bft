/*
 * Copyright (c) 2019 VMware, Inc. All rights reserved. VMware Confidential
 */

package com.vmware.concord.agent;

import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.RestController;

import com.vmware.blockchain.agent.ConcordAgent;

/**
 * AgentRestController --
 * This class provides the REST APIs to gRPC agent server.
 */
@RestController
public class AgentRestController {

    /**
     * status --
     * Get the status of Agent node.
     */
    @GetMapping("/api/status/")
    ConcordAgent.ConcordAgentStatus status() {
        return ConcordAgent.ConcordAgentStatus.newBuilder()
                           .setVersion(1)
                           .setStatus(ConcordAgent.ConcordAgentStatus.HealthStatus.GREEN)
                           .build();
    }
}

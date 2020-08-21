/*
 * Copyright (c) 2019 VMware, Inc. All rights reserved. VMware Confidential
 */

package com.vmware.blockchain.agent.services.node.status;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestMethod;
import org.springframework.web.bind.annotation.RestController;

import com.github.dockerjava.core.DockerClientBuilder;
import com.vmware.blockchain.agent.services.AgentDockerClient;
import com.vmware.blockchain.agent.services.NodeStartupOrchestrator;
import com.vmware.blockchain.deployment.v1.ConcordAgentConfiguration;

import lombok.extern.slf4j.Slf4j;

/**
 * Controller to change state of node components.
 */
@RestController
@Slf4j
public class NodeComponentController {

    private final AgentDockerClient agentDockerClient;
    private final ConcordAgentConfiguration configuration;
    private final NodeStartupOrchestrator nodeStartupOrchestrator;
    private final NodeComponentHealthUtil nodeComponentHealthUtil;

    private final String healthy = "HEALTHY";
    private final String unhealthy = "UNHEALTHY";

    @Autowired
    public NodeComponentController(AgentDockerClient agentDockerClient,
                                   ConcordAgentConfiguration configuration,
                                   NodeStartupOrchestrator nodeStartupOrchestrator,
                                   NodeComponentHealthUtil nodeComponentHealthUtil) {
        this.agentDockerClient = agentDockerClient;
        this.configuration = configuration;
        this.nodeStartupOrchestrator = nodeStartupOrchestrator;
        this.nodeComponentHealthUtil = nodeComponentHealthUtil;
    }

    /**
     * Start all the components of the node.
     */
    @RequestMapping(path = "/api/node/start", method = RequestMethod.POST)
    ResponseEntity<Void> startNodeComponents() {
        log.info("Received request to start node components...");

        nodeStartupOrchestrator.getComponents()
                .forEach(container -> {
                    var dockerClient = DockerClientBuilder.getInstance().build();
                    var containerResponse = agentDockerClient.inspectContainer(dockerClient,
                                                                               container.getContainerName());
                    agentDockerClient.startComponent(dockerClient, container, containerResponse.getId());
                });
        return new ResponseEntity<>(HttpStatus.OK);
    }

    /**
     * Stop all the components of the node.
     */
    @RequestMapping(path = "/api/node/stop", method = RequestMethod.POST)
    ResponseEntity<Void> stopNodeComponents() {
        log.info("Received request to stop node components...");

        nodeStartupOrchestrator.getComponents()
                .forEach(container -> {
                    var dockerClient = DockerClientBuilder.getInstance().build();
                    var containerResponse = agentDockerClient.inspectContainer(dockerClient,
                                                                               container.getContainerName());
                    agentDockerClient.stopComponent(dockerClient, container, containerResponse.getId());
                });
        return new ResponseEntity<>(HttpStatus.OK);
    }

    @RequestMapping(path = "/api/health/concord", method = RequestMethod.GET)
    ResponseEntity<NodeStatusResponse> getConcordHealth() {
        NodeStatusResponse response;
        if (this.nodeComponentHealthUtil.getConcordHealth()) {
            response = NodeStatusResponse.builder().status(healthy).build();
        } else {
            response = NodeStatusResponse.builder().status(unhealthy).build();
        }
        return new ResponseEntity<>(response, HttpStatus.OK);
    }

    @RequestMapping(path = "/api/health/daml", method = RequestMethod.GET)
    ResponseEntity<NodeStatusResponse> getDamlHealth() {
        NodeStatusResponse response;
        if (this.nodeComponentHealthUtil.getDamlHealth(configuration.getModel().getNodeType())) {
            response = NodeStatusResponse.builder().status(healthy).build();
        } else {
            response = NodeStatusResponse.builder().status(unhealthy).build();
        }
        return new ResponseEntity<>(response, HttpStatus.OK);
    }
}
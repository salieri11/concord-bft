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
import com.vmware.blockchain.agent.services.node.health.HealthCheckScheduler;
import com.vmware.blockchain.agent.services.node.health.HealthStatusResponse;
import com.vmware.blockchain.agent.services.node.health.NodeComponentHealthFactory;
import com.vmware.blockchain.deployment.v1.ConcordAgentConfiguration;
import com.vmware.blockchain.deployment.v1.ConcordComponent;
import com.vmware.blockchain.deployment.v1.ConcordModelSpecification;

import lombok.extern.slf4j.Slf4j;

/**
 * Controller to change state of node components.
 */
@RestController
@Slf4j
public class NodeComponentController {

    private final AgentDockerClient agentDockerClient;
    private final ConcordAgentConfiguration concordAgentConfiguration;
    private final NodeStartupOrchestrator nodeStartupOrchestrator;
    private final NodeComponentHealthFactory nodeComponentHealthFactory;
    private final HealthCheckScheduler healthCheckScheduler;

    @Autowired
    public NodeComponentController(AgentDockerClient agentDockerClient,
                                   ConcordAgentConfiguration concordAgentConfiguration,
                                   NodeStartupOrchestrator nodeStartupOrchestrator,
                                   NodeComponentHealthFactory nodeComponentHealthFactory,
                                   HealthCheckScheduler healthCheckScheduler) {
        this.agentDockerClient = agentDockerClient;
        this.concordAgentConfiguration = concordAgentConfiguration;
        this.nodeStartupOrchestrator = nodeStartupOrchestrator;
        this.nodeComponentHealthFactory = nodeComponentHealthFactory;
        this.healthCheckScheduler = healthCheckScheduler;
    }

    /**
     * Start all the components of the node.
     */
    @RequestMapping(path = "/api/node/start", method = RequestMethod.POST)
    ResponseEntity<Void> startNodeComponents() {
        log.info("Received request to start node components...");
        try {
            log.info("Starting healthchecks...");
            nodeComponentHealthFactory.initHealthChecks(nodeStartupOrchestrator.getContainerNetworkName());
            healthCheckScheduler.startHealthCheck();
        } catch (Exception ex) {
            log.error("Exception in starting healthchecks on node components.\n{}", ex);
            log.info("Node component start activity will continue without healthchecks..");
        }
        try {
            log.info("Starting components...");
            nodeStartupOrchestrator.getComponents()
                    .forEach(container -> {
                        var dockerClient = DockerClientBuilder.getInstance().build();
                        var containerResponse = agentDockerClient.inspectContainer(dockerClient,
                                container.getContainerName());
                        agentDockerClient.startComponent(dockerClient, container, containerResponse.getId());
                    });
            return new ResponseEntity<>(HttpStatus.OK);
        } catch (Exception ex) {
            log.error("Exception in starting node components.\n{}", ex);
            return new ResponseEntity<>(HttpStatus.EXPECTATION_FAILED);
        }
    }

    /**
     * Stop all the components of the node.
     */
    @RequestMapping(path = "/api/node/stop", method = RequestMethod.POST)
    ResponseEntity<Void> stopNodeComponents() {
        log.info("Received request to stop node components...");

        try {
            log.info("Stopping healthchecks...");
            healthCheckScheduler.stopHealthCheck();
            nodeComponentHealthFactory.tearDownHealthChecks();
        } catch (Exception ex) {
            log.error("Exception in stopping healthchecks on node components.\n{}", ex);
            log.info("Node component stop activity will continue..");
        }
        try {
            log.info("Stopping components...");
            nodeStartupOrchestrator.getComponents()
                    .forEach(container -> {
                        var dockerClient = DockerClientBuilder.getInstance().build();
                        var containerResponse = agentDockerClient.inspectContainer(dockerClient,
                                container.getContainerName());
                        agentDockerClient.stopComponent(dockerClient, container, containerResponse.getId());
                    });
            return new ResponseEntity<>(HttpStatus.OK);
        } catch (Exception ex) {
            log.error("Exception in stopping node components.\n{}", ex);
            return new ResponseEntity<>(HttpStatus.EXPECTATION_FAILED);
        }
    }

    @RequestMapping(path = "/api/health/concord", method = RequestMethod.GET)
    ResponseEntity<HealthStatusResponse> getConcordHealth() {
        log.info("Receieved request to query concord health...");
        return getHealthResponse(ConcordComponent.ServiceType.CONCORD);
    }

    @RequestMapping(path = "/api/health/daml", method = RequestMethod.GET)
    ResponseEntity<HealthStatusResponse> getDamlHealth() {
        log.info("Receieved request to query daml health...");
        ConcordComponent.ServiceType serviceType = concordAgentConfiguration.getModel().getNodeType()
                .equals(ConcordModelSpecification.NodeType.DAML_COMMITTER)
                ? ConcordComponent.ServiceType.DAML_EXECUTION_ENGINE
                : ConcordComponent.ServiceType.DAML_LEDGER_API;
        log.info("Daml service type on node: {}", serviceType);
        return getHealthResponse(serviceType);
    }

    @RequestMapping(path = "/api/health/stop", method = RequestMethod.POST)
    ResponseEntity<String> stopHealthCheck() {
        log.info("Receieved manual request to suspend health check...");
        try {
            healthCheckScheduler.stopHealthCheck();
            return new ResponseEntity<>("Health check suspended.\n", HttpStatus.OK);
        } catch (Exception ex) {
            return new ResponseEntity<>(ex.getLocalizedMessage(), HttpStatus.INTERNAL_SERVER_ERROR);
        }
    }

    @RequestMapping(path = "/api/health/start", method = RequestMethod.POST)
    ResponseEntity<String> startHealthCheck() {
        log.info("Receieved manual request to resume health check...");
        try {
            healthCheckScheduler.startHealthCheck();
            return new ResponseEntity<>("Health check resumed.\n", HttpStatus.OK);
        } catch (Exception ex) {
            return new ResponseEntity<>(ex.getLocalizedMessage(), HttpStatus.INTERNAL_SERVER_ERROR);
        }
    }

    private ResponseEntity<HealthStatusResponse> getHealthResponse(ConcordComponent.ServiceType serviceType) {
        try {
            return new ResponseEntity<HealthStatusResponse>(
                    nodeComponentHealthFactory.getHealthComponent(serviceType).getHealth(), HttpStatus.OK);
        } catch (Exception e) {
            return new ResponseEntity<HealthStatusResponse>(
                    HealthStatusResponse.builder().exception(e.getLocalizedMessage()).build(),
                    HttpStatus.INTERNAL_SERVER_ERROR);
        }
    }
}
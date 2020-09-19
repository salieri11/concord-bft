/*
 * Copyright (c) 2020 VMware, Inc. All rights reserved. VMware Confidential
 */

package com.vmware.blockchain.agent.services.node.health;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

import com.github.dockerjava.api.DockerClient;
import com.github.dockerjava.core.DockerClientBuilder;
import com.vmware.blockchain.agent.services.AgentDockerClient;
import com.vmware.blockchain.agent.services.node.health.concord.ConcordHealth;
import com.vmware.blockchain.agent.services.node.health.concord.ConcordHealthServiceInvoker;
import com.vmware.blockchain.agent.services.node.health.daml.DamlHealth;
import com.vmware.blockchain.agent.services.node.health.daml.DamlHealthServiceInvoker;
import com.vmware.blockchain.deployment.v1.ConcordAgentConfiguration;
import com.vmware.blockchain.deployment.v1.ConcordComponent;
import com.vmware.blockchain.deployment.v1.ConcordModelSpecification;

import lombok.extern.slf4j.Slf4j;

/**
 * Factory class to monitor health of components.
 */
@Slf4j
@Component
public class NodeComponentHealthFactory {

    private final ConcordAgentConfiguration concordAgentConfiguration;
    private final DamlHealthServiceInvoker damlHealthServiceInvoker;
    private final ConcordHealthServiceInvoker concordHealthServiceInvoker;
    private final AgentDockerClient agentDockerClient;

    private final DockerClient dockerClient = DockerClientBuilder.getInstance().build();

    @Autowired
    NodeComponentHealthFactory(ConcordAgentConfiguration concordAgentConfiguration,
                               DamlHealthServiceInvoker damlHealthServiceInvoker,
                               ConcordHealthServiceInvoker concordHealthServiceInvoker,
                               AgentDockerClient agentDockerClient) {
        this.concordAgentConfiguration = concordAgentConfiguration;
        this.damlHealthServiceInvoker = damlHealthServiceInvoker;
        this.concordHealthServiceInvoker = concordHealthServiceInvoker;
        this.agentDockerClient = agentDockerClient;
    }

    /**
     * get health component.
     * @param serviceType service type
     * @return {@link ComponentHealth}
     */
    public ComponentHealth getHealthComponent(ConcordComponent.ServiceType serviceType)
            throws UnsupportedOperationException, IllegalArgumentException {

        switch (getConcordAgentConfiguration().getModel().getBlockchainType()) {
            case ETHEREUM:
                if (!serviceType.equals(ConcordComponent.ServiceType.CONCORD)) {
                    throw new IllegalArgumentException("Blockchain type "
                            +  getConcordAgentConfiguration().getModel().getBlockchainType().name()
                            + " can not have service type "
                            + serviceType.name());
                }
                return getComponent(serviceType);
            case DAML:
                if (serviceType.equals(ConcordComponent.ServiceType.DAML_INDEX_DB)) {
                    throw new UnsupportedOperationException(
                            "Method unimplemented for service type " + serviceType.name());
                }
                if (getConcordAgentConfiguration().getModel().getNodeType()
                        .equals(ConcordModelSpecification.NodeType.DAML_PARTICIPANT)
                        && serviceType.equals(ConcordComponent.ServiceType.DAML_LEDGER_API)) {
                    return getComponent(serviceType);
                }
                if (getConcordAgentConfiguration().getModel().getNodeType()
                        .equals(ConcordModelSpecification.NodeType.DAML_COMMITTER)
                        && (serviceType.equals(ConcordComponent.ServiceType.DAML_EXECUTION_ENGINE)
                        || serviceType.equals(ConcordComponent.ServiceType.CONCORD))) {
                    return getComponent(serviceType);
                }
                throw new IllegalArgumentException("Node type "
                        + getConcordAgentConfiguration().getModel().getNodeType().name()
                        + " can not have service type " + serviceType.name());
            default:
                throw new UnsupportedOperationException(
                        "Method unimplemented for blockchain type " + getConcordAgentConfiguration().getModel()
                                .getBlockchainType().name());
        }
    }

    ComponentHealth getComponent(ConcordComponent.ServiceType serviceType)
            throws UnsupportedOperationException {
        switch (serviceType) {
            case CONCORD:
                log.info("Invoking {}", ConcordHealth.class.getName());
                return new ConcordHealth(concordHealthServiceInvoker);
            case DAML_LEDGER_API:
            case DAML_EXECUTION_ENGINE:
                log.info("Invoking {}", DamlHealth.class.getName());
                return new DamlHealth(damlHealthServiceInvoker);
            default:
                throw new UnsupportedOperationException(
                        "Method not implemented for service type " + serviceType.name());
        }
    }

    /**
     * initializes health check components.
     * TODO: to be also used in node start workflow.
     * @param containerNetworkName container network name.
     */
    public void initHealthChecks(String containerNetworkName) {
        if (getConcordAgentConfiguration()
                .getModel().getBlockchainType().equals(ConcordModelSpecification.BlockchainType.DAML)) {
            if (getConcordAgentConfiguration().getModel().getNodeType()
                    .equals(ConcordModelSpecification.NodeType.DAML_PARTICIPANT)) {
                String host = getHost(DamlHealthServiceInvoker.Service.PARTICIPANT.getHost(), containerNetworkName);
                damlHealthServiceInvoker.start(host);
            }
            if (getConcordAgentConfiguration().getModel().getNodeType()
                    .equals(ConcordModelSpecification.NodeType.DAML_COMMITTER)) {
                String host = getHost(DamlHealthServiceInvoker.Service.COMMITTER.getHost(),
                        containerNetworkName);
                damlHealthServiceInvoker.start(host);
                host = getHost(concordHealthServiceInvoker.getContainer(),
                        containerNetworkName);
                concordHealthServiceInvoker.registerHost(host);
            }
        } else {
            concordHealthServiceInvoker.registerHost(getHost(concordHealthServiceInvoker.getContainer(),
                    containerNetworkName));
        }
    }

    /**
     * tears down component health checks.
     */
    public void tearDownHealthChecks() throws InterruptedException {
        if (getConcordAgentConfiguration()
                .getModel().getBlockchainType() == ConcordModelSpecification.BlockchainType.DAML) {
            if (getConcordAgentConfiguration().getModel().getNodeType()
                    .equals(ConcordModelSpecification.NodeType.DAML_PARTICIPANT)) {
                damlHealthServiceInvoker.shutdown();
            }
            if (getConcordAgentConfiguration().getModel().getNodeType()
                    .equals(ConcordModelSpecification.NodeType.DAML_COMMITTER)) {
                damlHealthServiceInvoker.shutdown();
                concordHealthServiceInvoker.closeSocket();
            }
        } else {
            concordHealthServiceInvoker.closeSocket();
        }
    }

    ConcordAgentConfiguration getConcordAgentConfiguration() {
        return this.concordAgentConfiguration;
    }

    String getHost(String containerName, String networkName) {
        // Since "_" is not a valid literal in hostname, we can not use container name
        var inspectContainerResponse = agentDockerClient.inspectContainer(this.dockerClient, containerName);
        return inspectContainerResponse.getNetworkSettings().getNetworks()
                .get(networkName).getIpAddress();
    }
}
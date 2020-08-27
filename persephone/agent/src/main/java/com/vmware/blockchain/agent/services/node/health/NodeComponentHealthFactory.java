/*
 * Copyright (c) 2020 VMware, Inc. All rights reserved. VMware Confidential
 */

package com.vmware.blockchain.agent.services.node.health;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

import com.vmware.blockchain.agent.services.node.health.concord.ConcordHealth;
import com.vmware.blockchain.agent.services.node.health.daml.DamlExecutionEngineHealth;
import com.vmware.blockchain.agent.services.node.health.daml.DamlLedgerApiHealth;
import com.vmware.blockchain.deployment.v1.ConcordAgentConfiguration;
import com.vmware.blockchain.deployment.v1.ConcordComponent;
import com.vmware.blockchain.deployment.v1.ConcordModelSpecification;

/**
 * Factory class to monitor health of components.
 */
@Component
public class NodeComponentHealthFactory {

    private final ConcordAgentConfiguration configuration;

    @Autowired
    NodeComponentHealthFactory(ConcordAgentConfiguration configuration) {
        this.configuration = configuration;
    }

    /**
     * get health component.
     * @param serviceType service type
     * @return {@link ComponentHealth}
     */
    public ComponentHealth getHealthComponent(ConcordComponent.ServiceType serviceType)
            throws UnsupportedOperationException, IllegalArgumentException {

        switch (configuration.getModel().getBlockchainType()) {
            case ETHEREUM:
                if (!serviceType.equals(ConcordComponent.ServiceType.CONCORD)) {
                    throw new IllegalArgumentException(String.format("Blockchain type {} can not have service type {}",
                            configuration.getModel().getBlockchainType().name(), serviceType));
                }
                return getComponent(serviceType);
            case DAML:
                if (configuration.getModel().getNodeType().equals(ConcordModelSpecification.NodeType.DAML_PARTICIPANT)
                        && serviceType.equals(ConcordComponent.ServiceType.CONCORD)) {
                    throw new IllegalArgumentException(String.format("Node type {} can not have service type {}",
                            configuration.getModel().getNodeType().name(), serviceType));
                }
                return getComponent(serviceType);
            default:
                throw new UnsupportedOperationException(
                        "Method unimplemented for blockchain type " + configuration.getModel()
                                .getBlockchainType().name());
        }
    }

    private ComponentHealth getComponent(ConcordComponent.ServiceType serviceType)
            throws UnsupportedOperationException {
        switch (serviceType) {
            case CONCORD:
                return new ConcordHealth();
            case DAML_LEDGER_API:
                return new DamlLedgerApiHealth();
            case DAML_EXECUTION_ENGINE:
                return new DamlExecutionEngineHealth();
            default:
                throw new UnsupportedOperationException(
                        "Method not implemented for service type " + serviceType.name());
        }
    }
}
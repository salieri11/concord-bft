/*
 * Copyright (c) 2020 VMware, Inc. All rights reserved. VMware Confidential
 */

package com.vmware.blockchain.agent.services.node.health;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

import com.vmware.blockchain.agent.services.node.health.concord.ConcordHealth;
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

    @Autowired
    NodeComponentHealthFactory(ConcordAgentConfiguration concordAgentConfiguration,
                               DamlHealthServiceInvoker damlHealthServiceInvoker) {
        this.concordAgentConfiguration = concordAgentConfiguration;
        this.damlHealthServiceInvoker = damlHealthServiceInvoker;
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
                return new ConcordHealth();
            case DAML_LEDGER_API:
            case DAML_EXECUTION_ENGINE:
                log.info("Invoking {}", DamlHealth.class.getName());
                return new DamlHealth(damlHealthServiceInvoker);
            default:
                throw new UnsupportedOperationException(
                        "Method not implemented for service type " + serviceType.name());
        }
    }

    ConcordAgentConfiguration getConcordAgentConfiguration() {
        return this.concordAgentConfiguration;
    }
}
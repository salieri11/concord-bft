/*
 * Copyright (c) 2020 VMware, Inc. All rights reserved. VMware Confidential
 */

package com.vmware.blockchain.agent.services.node.health;

import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.springframework.boot.test.mock.mockito.MockBean;
import org.springframework.test.context.ContextConfiguration;
import org.springframework.test.context.junit.jupiter.SpringExtension;

import com.vmware.blockchain.agent.utils.AgentTestConfiguration;
import com.vmware.blockchain.deployment.v1.ConcordAgentConfiguration;
import com.vmware.blockchain.deployment.v1.ConcordComponent;
import com.vmware.blockchain.deployment.v1.ConcordModelSpecification;

/**
 * Test for {link @NodeComponentHealthFactory}.
 */
@ExtendWith(SpringExtension.class)
@ContextConfiguration(classes = {AgentTestConfiguration.class})
public class NodeComponentHealthFactoryTest {

    @MockBean
    private NodeComponentHealthFactory nodeComponentHealthFactory;

    private final ConcordAgentConfiguration configuration = mock(ConcordAgentConfiguration.class);
    private final ComponentHealth mockComponentHealth = mock(ComponentHealth.class);
    private final ConcordModelSpecification model = mock(ConcordModelSpecification.class);

    @BeforeEach
    void init() {
        when(nodeComponentHealthFactory.getConcordAgentConfiguration()).thenReturn(configuration);
        when(configuration.getModel()).thenReturn(model);

        when(nodeComponentHealthFactory.getComponent(any())).thenReturn(mockComponentHealth);
        when(nodeComponentHealthFactory.getHealthComponent(any())).thenCallRealMethod();
    }

    @Test
    void getHealthComponentDamlParticipantTestWhenServiceTypeConcord() {
        when(model.getBlockchainType()).thenReturn(ConcordModelSpecification.BlockchainType.DAML);
        when(model.getNodeType()).thenReturn(ConcordModelSpecification.NodeType.DAML_PARTICIPANT);
        Exception exception = Assertions.assertThrows(IllegalArgumentException.class, () -> {
            nodeComponentHealthFactory.getHealthComponent(ConcordComponent.ServiceType.CONCORD);
        });

        Assertions.assertEquals("Node type "
                + ConcordModelSpecification.NodeType.DAML_PARTICIPANT
                + " can not have service type "
                + ConcordComponent.ServiceType.CONCORD,
                exception.getLocalizedMessage());
    }

    @Test
    void getHealthComponentEthereumWhenServiceTypeDaml() {
        when(model.getBlockchainType()).thenReturn(ConcordModelSpecification.BlockchainType.ETHEREUM);
        Exception exception = Assertions.assertThrows(IllegalArgumentException.class, () -> {
            nodeComponentHealthFactory.getHealthComponent(ConcordComponent.ServiceType.DAML_EXECUTION_ENGINE);
        });

        Assertions.assertEquals("Blockchain type "
                + ConcordModelSpecification.BlockchainType.ETHEREUM
                + " can not have service type "
                + ConcordComponent.ServiceType.DAML_EXECUTION_ENGINE,
                exception.getLocalizedMessage());
    }

    @Test
    void getHealthComponentDamlParticipantTestWhenServiceTypeExecutionEngine() {
        when(model.getBlockchainType()).thenReturn(ConcordModelSpecification.BlockchainType.DAML);
        when(model.getNodeType()).thenReturn(ConcordModelSpecification.NodeType.DAML_PARTICIPANT);
        Exception exception = Assertions.assertThrows(IllegalArgumentException.class, () -> {
            nodeComponentHealthFactory.getHealthComponent(ConcordComponent.ServiceType.DAML_EXECUTION_ENGINE);
        });

        Assertions.assertEquals("Node type "
                + ConcordModelSpecification.NodeType.DAML_PARTICIPANT
                + " can not have service type "
                + ConcordComponent.ServiceType.DAML_EXECUTION_ENGINE,
                exception.getLocalizedMessage());
    }

    @Test
    void getHealthComponentDamlLedgerApiOnCommitter() {
        when(model.getBlockchainType()).thenReturn(ConcordModelSpecification.BlockchainType.DAML);
        when(model.getNodeType()).thenReturn(ConcordModelSpecification.NodeType.DAML_COMMITTER);
        Exception exception = Assertions.assertThrows(IllegalArgumentException.class, () -> {
            nodeComponentHealthFactory.getHealthComponent(ConcordComponent.ServiceType.DAML_LEDGER_API);
        });

        Assertions.assertEquals("Node type "
                + ConcordModelSpecification.NodeType.DAML_COMMITTER
                + " can not have service type "
                + ConcordComponent.ServiceType.DAML_LEDGER_API,
                exception.getLocalizedMessage());
    }

    @Test
    void getHealthComponentDamlIndexDb() {
        when(model.getBlockchainType()).thenReturn(ConcordModelSpecification.BlockchainType.DAML);
        when(model.getNodeType()).thenReturn(ConcordModelSpecification.NodeType.DAML_PARTICIPANT);
        Exception exception = Assertions.assertThrows(UnsupportedOperationException.class, () -> {
            nodeComponentHealthFactory.getHealthComponent(ConcordComponent.ServiceType.DAML_INDEX_DB);
        });

        Assertions.assertEquals("Method unimplemented for service type "
                + ConcordComponent.ServiceType.DAML_INDEX_DB,
                exception.getLocalizedMessage());
    }

    @Test
    void getHealthComponentPositiveDamlConcord() {
        when(model.getBlockchainType()).thenReturn(ConcordModelSpecification.BlockchainType.DAML);
        when(model.getNodeType()).thenReturn(ConcordModelSpecification.NodeType.DAML_COMMITTER);
        Assertions.assertEquals(mockComponentHealth,
                nodeComponentHealthFactory.getHealthComponent(ConcordComponent.ServiceType.CONCORD));
    }

    @Test
    void getHealthComponentPositiveEthereumConcord() {
        when(model.getBlockchainType()).thenReturn(ConcordModelSpecification.BlockchainType.ETHEREUM);
        when(model.getNodeType()).thenReturn(ConcordModelSpecification.NodeType.NONE);
        Assertions.assertEquals(mockComponentHealth,
                nodeComponentHealthFactory.getHealthComponent(ConcordComponent.ServiceType.CONCORD));
    }

    @Test
    void getHealthComponentPositiveDamlCommitter() {
        when(model.getBlockchainType()).thenReturn(ConcordModelSpecification.BlockchainType.DAML);
        when(model.getNodeType()).thenReturn(ConcordModelSpecification.NodeType.DAML_COMMITTER);
        Assertions.assertEquals(mockComponentHealth,
                nodeComponentHealthFactory.getHealthComponent(ConcordComponent.ServiceType.DAML_EXECUTION_ENGINE));
    }

    @Test
    void getHealthComponentPositiveDamlParticipant() {
        when(model.getBlockchainType()).thenReturn(ConcordModelSpecification.BlockchainType.DAML);
        when(model.getNodeType()).thenReturn(ConcordModelSpecification.NodeType.DAML_PARTICIPANT);
        Assertions.assertEquals(mockComponentHealth,
                nodeComponentHealthFactory.getHealthComponent(ConcordComponent.ServiceType.DAML_LEDGER_API));
    }
}

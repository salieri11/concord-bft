/*
 * Copyright (c) 2020 VMware, Inc. All rights reserved. VMware Confidential
 */

package com.vmware.blockchain.agent.services.node.health;

import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.ArgumentMatchers.isA;
import static org.mockito.Mockito.doCallRealMethod;
import static org.mockito.Mockito.doNothing;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;


import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.springframework.boot.test.mock.mockito.MockBean;
import org.springframework.test.context.ContextConfiguration;
import org.springframework.test.context.junit.jupiter.SpringExtension;
import org.springframework.test.util.ReflectionTestUtils;

import com.vmware.blockchain.agent.services.node.health.concord.ConcordHealthServiceInvoker;
import com.vmware.blockchain.agent.services.node.health.daml.DamlHealthServiceInvoker;
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
    private final DamlHealthServiceInvoker damlHealthServiceInvoker = mock(DamlHealthServiceInvoker.class);
    private final ConcordHealthServiceInvoker concordHealthServiceInvoker = mock(ConcordHealthServiceInvoker.class);

    @BeforeEach
    void init() throws InterruptedException {
        when(nodeComponentHealthFactory.getConcordAgentConfiguration()).thenReturn(configuration);
        when(configuration.getModel()).thenReturn(model);
        when(nodeComponentHealthFactory.getComponent(any())).thenReturn(mockComponentHealth);
        when(nodeComponentHealthFactory.getHost(any(), any())).thenReturn("concord");
        when(nodeComponentHealthFactory.getHost(eq("daml_ledger_api"), any())).thenReturn("daml_ledger_api");
        when(nodeComponentHealthFactory.getHost(eq("daml_execution_engine"), any()))
                .thenReturn("daml_execution_engine");
        doNothing().when(damlHealthServiceInvoker).shutdown();
        doNothing().when(damlHealthServiceInvoker).start(isA(String.class));
        doNothing().when(concordHealthServiceInvoker).closeSocket();
        doNothing().when(concordHealthServiceInvoker).createSocket(isA(String.class));

        when(nodeComponentHealthFactory.getHealthComponent(any())).thenCallRealMethod();
        doCallRealMethod().when(nodeComponentHealthFactory).initHealthChecks(any());
        doCallRealMethod().when(nodeComponentHealthFactory).tearDownHealthChecks(any());

        ReflectionTestUtils.setField(nodeComponentHealthFactory,
                "damlHealthServiceInvoker", damlHealthServiceInvoker);
        ReflectionTestUtils.setField(nodeComponentHealthFactory,
                "concordHealthServiceInvoker", concordHealthServiceInvoker);
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

    @Test
    void initHealthChecksDamlCommitter() {
        when(model.getBlockchainType()).thenReturn(ConcordModelSpecification.BlockchainType.DAML);
        when(model.getNodeType()).thenReturn(ConcordModelSpecification.NodeType.DAML_COMMITTER);
        nodeComponentHealthFactory.initHealthChecks(any());
        verify(damlHealthServiceInvoker, times(1)).start("daml_execution_engine");
        verify(concordHealthServiceInvoker, times(1)).createSocket("concord");
    }

    @Test
    void initHealthChecksDamlParticipant() {
        when(model.getBlockchainType()).thenReturn(ConcordModelSpecification.BlockchainType.DAML);
        when(model.getNodeType()).thenReturn(ConcordModelSpecification.NodeType.DAML_PARTICIPANT);
        nodeComponentHealthFactory.initHealthChecks(any());
        verify(damlHealthServiceInvoker, times(1)).start("daml_ledger_api");
        verify(concordHealthServiceInvoker, never()).createSocket(any());
    }

    @Test
    void initHealthChecksEthereum() {
        when(model.getBlockchainType()).thenReturn(ConcordModelSpecification.BlockchainType.ETHEREUM);
        nodeComponentHealthFactory.initHealthChecks(any());
        verify(damlHealthServiceInvoker, never()).start(any());
        verify(concordHealthServiceInvoker, times(1)).createSocket("concord");
    }

    @Test
    void tearDownHealthChecksDamlCommitter() throws InterruptedException {
        when(model.getBlockchainType()).thenReturn(ConcordModelSpecification.BlockchainType.DAML);
        when(model.getNodeType()).thenReturn(ConcordModelSpecification.NodeType.DAML_COMMITTER);
        nodeComponentHealthFactory.tearDownHealthChecks(any());
        verify(damlHealthServiceInvoker, times(1)).shutdown();
        verify(concordHealthServiceInvoker, times(1)).closeSocket();
    }

    @Test
    void tearDownHealthChecksDamlParticipant() throws InterruptedException {
        when(model.getBlockchainType()).thenReturn(ConcordModelSpecification.BlockchainType.DAML);
        when(model.getNodeType()).thenReturn(ConcordModelSpecification.NodeType.DAML_PARTICIPANT);
        nodeComponentHealthFactory.tearDownHealthChecks(any());
        verify(damlHealthServiceInvoker, times(1)).shutdown();
        verify(concordHealthServiceInvoker, never()).closeSocket();
    }

    @Test
    void tearDownHealthChecksEthereum() throws InterruptedException {
        when(model.getBlockchainType()).thenReturn(ConcordModelSpecification.BlockchainType.ETHEREUM);
        nodeComponentHealthFactory.tearDownHealthChecks(any());
        verify(damlHealthServiceInvoker, never()).shutdown();
        verify(concordHealthServiceInvoker, times(1)).closeSocket();
    }
}

/*
 * Copyright (c) 2018-2019 VMware, Inc. All rights reserved. VMware Confidential
 */

package com.vmware.blockchain.services.blockchains;

import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.when;

import java.util.List;
import java.util.UUID;
import java.util.stream.Collectors;

import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.ArgumentCaptor;
import org.mockito.Captor;
import org.springframework.boot.test.mock.mockito.MockBean;
import org.springframework.context.ApplicationEventPublisher;
import org.springframework.test.context.junit.jupiter.SpringExtension;

import com.google.common.collect.ImmutableList;
import com.vmware.blockchain.connections.ConnectionPoolManager;
import com.vmware.blockchain.dao.GenericDao;
import com.vmware.blockchain.services.blockchains.Blockchain.NodeEntry;
import com.vmware.blockchain.services.profiles.Consortium;

/**
 * Test the blockchain manager.
 * Need mock beans for BlockchainRepository and for the ApplicationPublisher.
 */
@ExtendWith(SpringExtension.class)
public class BlockchainServiceTest {
    @MockBean
    private GenericDao genericDao;

    @MockBean
    private ApplicationEventPublisher publisher;

    @MockBean
    private ConnectionPoolManager connectionPoolManager;

    private BlockchainService manager;

    @Captor
    private ArgumentCaptor<BlockchainManagerEvent> eventCaptor;

    /**
     * Initialize mocks.
     */
    @BeforeEach
    void init() {
        // Trying to autowire pulls in too many things
        manager = new BlockchainService(genericDao, publisher, connectionPoolManager);
        when(genericDao.put(any(Blockchain.class), any())).thenAnswer(invocation -> {
            Blockchain b = invocation.getArgument(0);
            if (b.getId() == null) {
                b.setId(UUID.fromString("1070aacc-6638-4267-8cc9-dbdc07235084"));
            }
            return b;
        });
    }

    @Test
    void testCreateAndUpdate() {
        Blockchain b = manager.create(new Consortium(), "ip4:20,ip2:30,ip3:40,ip1:50",
                                      "a=ip4:20,b=ip2:30, c=ip3:40,d=ip1:50",
                                      "a=src/test/resources/certs/acert.txt,b=src/test/resources/certs/bcert.txt, "
                                      + "c=src/test/resources/certs/ccert.txt,d=src/test/resources/certs/dcert.txt");

        List<String> expectedIps = ImmutableList.of("ip4:20", "ip2:30", "ip3:40", "ip1:50");
        List<String> expectedUrls = ImmutableList.of("ip4:20", "ip2:30", "ip3:40", "ip1:50");
        List<String> expectedCerts = ImmutableList.of("Cert A", "Cert B", "Cert C", "Cert D");

        List<String> actualIps = b.getNodeList().stream().map(n -> n.getIp()).collect(Collectors.toList());
        List<String> actualUrls = b.getNodeList().stream().map(n -> n.getUrl()).collect(Collectors.toList());
        List<String> actualCerts = b.getNodeList().stream().map(n -> n.getCert().strip()).collect(Collectors.toList());

        Blockchain.BlockchainState expectedBlockchainState = Blockchain.BlockchainState.ACTIVE;
        Blockchain.BlockchainState actualBlockchainState = b.getState();

        Assertions.assertEquals(expectedBlockchainState, actualBlockchainState);
        Assertions.assertEquals(expectedIps, actualIps);
        Assertions.assertEquals(expectedUrls, actualUrls);
        Assertions.assertEquals(expectedCerts, actualCerts);
    }

    @Test
    void testCreateNode() {
        NodeEntry node = new NodeEntry(UUID.fromString("d1740514-606b-4e88-ab54-fc8182630890"), "1.2.3.4",
                                       "http://localhost", "", UUID.fromString("2dd3386d-69f4-4fcf-9fc2-42607929c0b8"));
        Assertions.assertEquals("1.2.3.4", node.getHostName());
    }

}

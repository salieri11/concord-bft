/*
 * Copyright (c) 2018-2019 VMware, Inc. All rights reserved. VMware Confidential
 */

package com.vmware.blockchain.services.profiles;

import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import java.util.Optional;
import java.util.UUID;

import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.ArgumentCaptor;
import org.mockito.Captor;
import org.springframework.boot.test.mock.mockito.MockBean;
import org.springframework.context.ApplicationEventPublisher;
import org.springframework.test.context.junit.jupiter.SpringExtension;

import com.vmware.blockchain.common.EntityModificationException;
import com.vmware.blockchain.services.profiles.BlockchainManagerEvent.Action;

/**
 * Test the blockchain manager.
 * Need mock beans for BlockchainRepository and for the ApplicationPublisher.
 */
@ExtendWith(SpringExtension.class)
class BlockchainManagerTest {
    @MockBean
    private BlockchainRepository repo;

    @MockBean
    private ApplicationEventPublisher publisher;

    private BlockchainManager manager;

    @Captor
    private ArgumentCaptor<BlockchainManagerEvent> eventCaptor;

    /**
     * Initialize mocks.
     */
    @BeforeEach
    void init() {
        // Trying to autowire pulls in too many things
        manager = new BlockchainManager(repo, publisher);
        when(repo.save(any(Blockchain.class))).thenAnswer(invocation -> {
            Blockchain b = invocation.getArgument(0);
            if (b.getId() == null) {
                b.setId(UUID.fromString("1070aacc-6638-4267-8cc9-dbdc07235084"));
            }
            return b;
        });
    }

    @Test
    void testCreateAndUpdate() {
        Blockchain b = manager.create("ip4:20,ip2:30, ip3:40,ip1:50");
        // make sure the list comes back sorted
        Assertions.assertEquals("ip1:50,ip2:30,ip3:40,ip4:20", b.getIpList());
        b.setIpList("ip1:50,ip2:30,ip3:40,ip4:20,ip0:30");
        Blockchain nb = manager.update(b);
        Assertions.assertEquals("ip0:30,ip1:50,ip2:30,ip3:40,ip4:20", nb.getIpList());
    }

    @Test
    void testAddNode() {
        Blockchain b = new Blockchain(UUID.fromString("dcc19799-8972-43f3-8f76-8e4736a8a9f9"),
                null, "ip4:20,ip2:30, ip3:40,ip1:50");
        when(repo.findById(UUID.fromString("dcc19799-8972-43f3-8f76-8e4736a8a9f9")))
            .thenAnswer(invocation -> Optional.of(b));
        Blockchain nb = manager.addNode(UUID.fromString("dcc19799-8972-43f3-8f76-8e4736a8a9f9"), "ip0:40");
        Assertions.assertEquals("ip0:40,ip1:50,ip2:30,ip3:40,ip4:20", nb.getIpList());
        verify(publisher).publishEvent(eventCaptor.capture());
        BlockchainManagerEvent event = eventCaptor.getValue();
        Assertions.assertEquals("ip0:40", event.getNode());
        Assertions.assertEquals(b, event.getBlockchain());
        Assertions.assertEquals(Action.ADD_NODE, event.getAction());
    }

    @Test
    void testDeleteNode() {
        Blockchain b = new Blockchain(UUID.fromString("dcc19799-8972-43f3-8f76-8e4736a8a9f9"),
                null, "ip4:20,ip2:30, ip3:40,ip1:50");
        when(repo.findById(UUID.fromString("dcc19799-8972-43f3-8f76-8e4736a8a9f9")))
            .thenAnswer(invocation -> Optional.of(b));
        Blockchain nb = manager.deleteNode(UUID.fromString("dcc19799-8972-43f3-8f76-8e4736a8a9f9"), "ip1:50");
        Assertions.assertEquals("ip2:30,ip3:40,ip4:20", nb.getIpList());
        verify(publisher).publishEvent(eventCaptor.capture());
        BlockchainManagerEvent event = eventCaptor.getValue();
        Assertions.assertEquals("ip1:50", event.getNode());
        Assertions.assertEquals(b, event.getBlockchain());
        Assertions.assertEquals(Action.DELETE_NODE, event.getAction());
    }

    @Test
    void testBadDeleteNode() {
        Blockchain b = new Blockchain(UUID.fromString("dcc19799-8972-43f3-8f76-8e4736a8a9f9"),
                null, "ip4:20,ip2:30, ip3:40,ip1:50");
        when(repo.findById(UUID.fromString("dcc19799-8972-43f3-8f76-8e4736a8a9f9")))
            .thenAnswer(invocation -> Optional.of(b));
        Assertions.assertThrows(
            EntityModificationException.class,
            () -> manager.deleteNode(UUID.fromString("dcc19799-8972-43f3-8f76-8e4736a8a9f9"), "ip0:50"));
    }

}

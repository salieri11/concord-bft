/*
 * Copyright (c) 2018-2019 VMware, Inc. All rights reserved. VMware Confidential
 */

package com.vmware.blockchain.services.blockchains;

import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.when;

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

import com.vmware.blockchain.dao.GenericDao;
import com.vmware.blockchain.services.blockchains.Blockchain.NodeEntry;

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

    private BlockchainService manager;

    @Captor
    private ArgumentCaptor<BlockchainManagerEvent> eventCaptor;

    /**
     * Initialize mocks.
     */
    @BeforeEach
    void init() {
        // Trying to autowire pulls in too many things
        manager = new BlockchainService(genericDao, publisher);
        when(genericDao.put(any(Blockchain.class), any())).thenAnswer(invocation -> {
            Blockchain b = invocation.getArgument(0);
            if (b.getId() == null) {
                b.setId(UUID.fromString("1070aacc-6638-4267-8cc9-dbdc07235084"));
            }
            return b;
        });
    }

    @Test
    void testCreateNode() {
        NodeEntry node = new NodeEntry(UUID.fromString("d1740514-606b-4e88-ab54-fc8182630890"), "1.2.3.4",
                                       "1.2.3.4",
                                       "http://localhost", "",
                                       UUID.fromString("2dd3386d-69f4-4fcf-9fc2-42607929c0b8"));
        Assertions.assertEquals("1.2.3.4", node.getHostName());
    }

}

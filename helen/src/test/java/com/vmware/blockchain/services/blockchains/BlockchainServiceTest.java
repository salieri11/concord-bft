/*
 * Copyright (c) 2018-2019 VMware, Inc. All rights reserved. VMware Confidential
 */

package com.vmware.blockchain.services.blockchains;

import static com.vmware.blockchain.services.blockchains.Blockchain.NodeEntry;
import static com.vmware.blockchain.services.blockchains.BlockchainApiObjects.BlockchainPatch;

import static org.junit.Assert.assertEquals;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.when;

import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
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
import com.vmware.blockchain.services.blockchains.replicas.Replica;
import com.vmware.blockchain.services.blockchains.replicas.ReplicaService;
import com.vmware.blockchain.services.blockchains.zones.ZoneService;

/**
 * Test the blockchain manager.
 * Need mock beans for BlockchainRepository and for the ApplicationPublisher.
 */
@ExtendWith(SpringExtension.class)
public class BlockchainServiceTest {

    private static final String blockchainVersion = "new version";
    private static final String execEngineVersion = "new EE version";

    @MockBean
    private GenericDao genericDao;

    @MockBean
    private ApplicationEventPublisher publisher;

    private BlockchainService manager;

    @Captor
    private ArgumentCaptor<BlockchainManagerEvent> eventCaptor;

    @MockBean
    private ReplicaService replicaService;

    @MockBean
    private ZoneService zoneService;

    /**
     * Initialize mocks.
     */
    @BeforeEach
    void init() {
        // Trying to autowire pulls in too many things
        manager = new BlockchainService(genericDao, publisher, replicaService, zoneService);
    }

    @Test
    void testCreateNode() {
        when(genericDao.put(any(Blockchain.class), any())).thenAnswer(invocation -> {
            Blockchain b = invocation.getArgument(0);
            if (b.getId() == null) {
                b.setId(UUID.fromString("1070aacc-6638-4267-8cc9-dbdc07235084"));
            }
            return b;
        });

        NodeEntry node = new NodeEntry(UUID.fromString("d1740514-606b-4e88-ab54-fc8182630890"), "1.2.3.4",
                                       "1.2.3.4",
                                       "http://localhost", "",
                                       UUID.fromString("2dd3386d-69f4-4fcf-9fc2-42607929c0b8"));
        Assertions.assertEquals("1.2.3.4", node.getHostName());
    }

    @Test
    public void testUpdate() {
        HashMap<String, String> metadata = new HashMap<>();
        metadata.put("concord", "1.0");
        Blockchain blockchain = new Blockchain(new UUID(1, 2), "NA test version", "NA exec eng version",
                                               Blockchain.BlockchainType.ETHEREUM, Blockchain.BlockchainState.INACTIVE,
                                               Collections.emptyList(), metadata, null);
        blockchain.setId(UUID.fromString("fb212e5a-0428-46f9-8faa-b3f15c9843e8"));

        BlockchainPatch patch = new BlockchainPatch();
        patch.setBlockchainVersion(blockchainVersion);
        patch.setExecutionEngineVersion(execEngineVersion);

        when(genericDao.put(blockchain, blockchain)).thenAnswer(invocation -> {
            Blockchain b = invocation.getArgument(0);
            b.setBlockchainVersion(blockchainVersion);
            b.setExecutionEngineVersion(execEngineVersion);
            return b;
        });

        Blockchain result = manager.update(blockchain, patch);

        assertEquals(result.blockchainVersion, blockchainVersion);
        assertEquals(result.executionEngineVersion, execEngineVersion);
    }

    @Test
    public void testGetBlockchainByTypeWithReplicas() {
        final Map<UUID, List<Replica>> blockchainIdAndReplicas =
                manager.getCloudBlockchainsWithReplicas(Blockchain.BlockchainType.ETHEREUM);
        Assertions.assertNotNull(blockchainIdAndReplicas);
    }

}

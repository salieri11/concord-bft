/*
 * Copyright (c) 2018-2019 VMware, Inc. All rights reserved. VMware Confidential
 */

package com.vmware.blockchain.services.profiles;

import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.when;

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

import com.google.common.collect.ImmutableMap;
import com.vmware.blockchain.connections.ConnectionPoolManager;
import com.vmware.blockchain.dao.GenericDao;

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
        Blockchain b = manager.create(new Consortium(), "ip4:20 , ip2:30  ,ip3:40 ,ip1:50",
                                      "a=ip4:20,b=ip2:30, c=ip3:40,d = ip1:50",
                                      "a=acert,b=bcert, c=ccert,d = dcert");

        Map<String, String> expectedUrls = new ImmutableMap.Builder<String, String>()
                .put("a", "ip4:20").put("b", "ip2:30").put("c", "ip3:40").put("d", "ip1:50")
                .build();
        Map<String, String> expectedCerts = new ImmutableMap.Builder<String, String>()
                .put("a", "acert").put("b", "bcert").put("c", "ccert").put("d", "dcert")
                .build();
        Assertions.assertEquals("ip4:20,ip2:30,ip3:40,ip1:50", b.getIpList());
        Assertions.assertEquals(expectedUrls, b.getUrlsAsMap());
        Assertions.assertEquals(expectedCerts, b.getCertsAsMap());
        b.setIpList("ip1:50 , ip2:30  ,ip3:40, ip4:20 \t,ip0:30");
        b.setRpcUrls("d=ip1:50,b=ip2:30 , c=ip3:40,a = ip4:20,e=ip0:30");
        b.setRpcCerts("d=dcert,b=bcert , c=cert,a = acert,e=ecert");
        Blockchain nb = manager.update(b);
        expectedUrls = new ImmutableMap.Builder<String, String>()
                .put("a", "ip4:20").put("b", "ip2:30").put("c", "ip3:40").put("d", "ip1:50").put("e", "ip0:30")
                .build();
        expectedCerts = new ImmutableMap.Builder<String, String>()
                .put("a", "acert").put("b", "bcert").put("c", "cert").put("d", "dcert").put("e", "ecert")
                .build();
        Assertions.assertEquals("ip1:50,ip2:30,ip3:40,ip4:20,ip0:30", nb.getIpList());
        Assertions.assertEquals(expectedUrls, nb.getUrlsAsMap());
        Assertions.assertEquals(expectedCerts, nb.getCertsAsMap());
    }


}

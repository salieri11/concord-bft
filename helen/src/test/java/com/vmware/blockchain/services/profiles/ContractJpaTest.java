/*
 * Copyright (c) 2019 VMware, Inc. All rights reserved. VMware Confidential
 */

package com.vmware.blockchain.services.profiles;

import java.util.List;
import java.util.UUID;

import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.autoconfigure.orm.jpa.DataJpaTest;
import org.springframework.test.context.ContextConfiguration;
import org.springframework.test.context.TestPropertySource;
import org.springframework.test.context.junit.jupiter.SpringExtension;

import com.vmware.blockchain.services.contracts.Contract;
import com.vmware.blockchain.services.contracts.ContractRepository;
import com.vmware.blockchain.services.utils.TestJpaConfig;

import io.zonky.test.db.AutoConfigureEmbeddedDatabase;

/**
 * Test the Contracts JPA calls.
 */
@ExtendWith(SpringExtension.class)
@TestPropertySource(locations = "classpath:db-postgres-test.properties")
@AutoConfigureEmbeddedDatabase
@DataJpaTest
@ContextConfiguration(classes = TestJpaConfig.class)
public class ContractJpaTest {

    @Autowired
    private BlockchainRepository bcRepo;

    @Autowired
    private ContractRepository ctRepo;

    @Autowired
    private ConsortiumRepository cnRepo;

    private Consortium consortium;
    private Blockchain blockchain1;
    private Blockchain blockchain2;
    private Contract contract1;
    private Contract contract2;
    private Contract contract3;
    UUID b1Id;
    UUID b2Id;

    @BeforeEach
    void init() throws Exception {
        consortium = new Consortium();
        consortium.setConsortiumName("Test Name");
        consortium.setConsortiumType("Test type");
        consortium = cnRepo.save(consortium);

        blockchain1 = new Blockchain();
        blockchain1.setIpList("1,2,3");
        blockchain1.setRpcUrls("a=1,b=2,c=3");
        blockchain1.setConsortium(consortium);
        blockchain1 = bcRepo.save(blockchain1);
        b1Id = blockchain1.getId();

        blockchain2 = new Blockchain();
        blockchain2.setIpList("4,5,6");
        blockchain2.setRpcUrls("d=4,e-5,f=6");
        blockchain2.setConsortium(consortium);
        blockchain2 = bcRepo.save(blockchain2);
        b2Id = blockchain2.getId();

        // embedded database doesn't handle sequence number right. CockroachDB does.
        contract1 = Contract.builder()
                .name("contract")
                .versionName("1")
                .blockchainId(blockchain1.getId())
                .seq(1)
                .build();
        contract1 = ctRepo.save(contract1);

        contract2 = Contract.builder()
                .name("contract")
                .versionName("1")
                .blockchainId(blockchain2.getId())
                .seq(2)
                .build();
        contract2 = ctRepo.save(contract2);

        contract3 = Contract.builder()
                .name("contract")
                .versionName("2")
                .blockchainId(blockchain2.getId())
                .seq(3)
                .build();
        contract3 = ctRepo.save(contract3);
    }

    @Test
    void testExists() throws Exception {
        Assertions.assertTrue(ctRepo.existsByNameAndBlockchainId("contract", b1Id));
        Assertions.assertTrue(ctRepo.existsByNameAndBlockchainId("contract", b2Id));
        Assertions.assertFalse(ctRepo.existsByNameAndBlockchainId("no contract", b1Id));
        Assertions.assertTrue(ctRepo.existsByNameAndVersionNameAndBlockchainId("contract", "1", b1Id));
        Assertions.assertTrue(ctRepo.existsByNameAndVersionNameAndBlockchainId("contract", "1", b2Id));
        Assertions.assertFalse(ctRepo.existsByNameAndVersionNameAndBlockchainId("contract", "2", b1Id));
        Assertions.assertTrue(ctRepo.existsByNameAndVersionNameAndBlockchainId("contract", "2", b2Id));
    }

    @Test
    void testFind() throws Exception {
        List<Contract> l1 = ctRepo.findByNameAndBlockchainId("contract", b1Id);
        List<Contract> l2 = ctRepo.findByNameAndBlockchainId("contract", b2Id);
        Assertions.assertEquals(1, l1.size());
        Assertions.assertEquals(2, l2.size());
    }

    @Test
    void testOrdered() throws Exception {
        List<Contract> l1 = ctRepo.findByNameAndBlockchainIdOrderBySeqDesc("contract", b1Id);
        List<Contract> l2 = ctRepo.findByNameAndBlockchainIdOrderBySeqDesc("contract", b2Id);
        Assertions.assertEquals(1, l1.size());
        Assertions.assertEquals(2, l2.size());
        Assertions.assertEquals("2", l2.get(0).getVersionName());
        Assertions.assertEquals("1", l2.get(1).getVersionName());
    }

    /*
     * I would like to add the following test, but it doesn't work in the embedded database. The issue is that the order
     * by variable must be present in the projection as well. In looking around, there appears to be some debate with or
     *  not this is the correct behavior of SQL.
     *
     * @Test void testDistinct() throws Exception { List<BriefContractProjection> l1 =
     * ctRepo.findDistinctBriefByBlockchainIdOrderBySeqDesc(b1Id); List<BriefContractProjection> l2 =
     * ctRepo.findDistinctBriefByBlockchainIdOrderBySeqDesc(b2Id); Assertions.assertEquals(1, l1.size());
     * Assertions.assertEquals(1, l2.size()); }
     */
}

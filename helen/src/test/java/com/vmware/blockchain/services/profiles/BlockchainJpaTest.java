/*
 * Copyright (c) 2018-2019 VMware, Inc. All rights reserved. VMware Confidential
 */

package com.vmware.blockchain.services.profiles;

import java.util.List;
import java.util.Map;
import java.util.Optional;

import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.autoconfigure.orm.jpa.DataJpaTest;
import org.springframework.test.context.TestPropertySource;
import org.springframework.test.context.jdbc.Sql;
import org.springframework.test.context.jdbc.Sql.ExecutionPhase;
import org.springframework.test.context.jdbc.SqlGroup;
import org.springframework.test.context.junit.jupiter.SpringExtension;

import com.google.common.collect.ImmutableMap;
import com.vmware.blockchain.services.contracts.Contract;
import com.vmware.blockchain.services.contracts.ContractRepository;

/**
 * Test the Blockchain JPA.
 */
@ExtendWith(SpringExtension.class)
@TestPropertySource(locations = "classpath:db-test.properties")
@SqlGroup({
    @Sql(executionPhase = ExecutionPhase.BEFORE_TEST_METHOD, scripts = "classpath:db-test-init.sql"),
    @Sql(executionPhase = ExecutionPhase.AFTER_TEST_METHOD, scripts = "classpath:db-test-cleanup.sql")
    })
@DataJpaTest
class BlockchainJpaTest {

    @Autowired
    private BlockchainRepository bcRepo;

    @Autowired
    private ConsortiumRepository cnRepo;

    @Autowired
    private ContractRepository ctRepo;

    private Consortium consortium;
    private Blockchain blockchain;
    private Contract contract;

    /**
     * Create a Consortium and a Blockchain, and save it.
     */
    @BeforeEach
    void init() throws Exception {
        consortium = new Consortium();
        consortium.setConsortiumName("Test Name");
        consortium.setConsortiumType("Test type");
        consortium = cnRepo.save(consortium);

        blockchain = new Blockchain();
        blockchain.setConsortium(consortium);
        blockchain.setIpList("1,2,3");
        blockchain.setRpcUrls("a=1,b=2,c=3");
        blockchain = bcRepo.save(blockchain);

        contract = Contract.builder()
                    .name("Contract")
                    .versionName("version 1")
                    .blockchainId(blockchain.getId())
                    .owner("owner").build();
        contract = ctRepo.save(contract);
        Optional<Contract> c = ctRepo.findById(contract.getId());
    }


    @Test
    void findByIdTest() {
        Optional<Blockchain> nbc = bcRepo.findById(blockchain.getId());
        Assertions.assertTrue(nbc.isPresent());
        Assertions.assertEquals(blockchain.getId(), nbc.get().getId());
    }

    @Test
    void findByConsortiumTest() {
        List<Blockchain> l = bcRepo.findAllByConsortium(consortium);
        Assertions.assertEquals(1, l.size());
        Assertions.assertEquals(blockchain.getId(), l.get(0).getId());
    }

    @Test
    void getAsListTest() {
        Map<String, String> expected = ImmutableMap.of("a", "1", "b", "2", "c", "3");
        Assertions.assertEquals(expected, blockchain.getUrlsAsMap());
    }

    @Test
    void updateListTest() {
        Map<String, String> expected = ImmutableMap.of("a", "1", "b", "2", "c", "3");
        blockchain.setUrlsAsMap(expected);
        Blockchain bc = bcRepo.save(blockchain);
        Assertions.assertEquals(expected, bc.getUrlsAsMap());
    }

}

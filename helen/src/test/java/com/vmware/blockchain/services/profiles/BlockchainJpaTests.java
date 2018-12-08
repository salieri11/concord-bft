/*
 * Copyright (c) 2018 VMware, Inc. All rights reserved. VMware Confidential
 */

package com.vmware.blockchain.services.profiles;

import java.util.List;
import java.util.Optional;

import org.junit.Assert;
import org.junit.Before;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.autoconfigure.orm.jpa.DataJpaTest;
import org.springframework.test.context.TestPropertySource;
import org.springframework.test.context.jdbc.Sql;
import org.springframework.test.context.jdbc.Sql.ExecutionPhase;
import org.springframework.test.context.jdbc.SqlGroup;
import org.springframework.test.context.junit4.SpringRunner;

import com.google.common.collect.ImmutableList;


/**
 * Test the Blockchain JPA.
 */
@RunWith(SpringRunner.class)
@TestPropertySource(locations = "classpath:db-test.properties")
@SqlGroup({
    @Sql(executionPhase = ExecutionPhase.BEFORE_TEST_METHOD, scripts = "classpath:db-test-init.sql"),
    @Sql(executionPhase = ExecutionPhase.AFTER_TEST_METHOD, scripts = "classpath:db-test-cleanup.sql")
    })
@DataJpaTest
public class BlockchainJpaTests {

    @Autowired
    private BlockchainRepository bcRepo;

    @Autowired
    private ConsortiumRepository cnRepo;

    private Consortium consortium;
    private Blockchain blockchain;

    /**
     * Create a Consortium and a Blockchain, and save it.
     */
    @Before
    public void init() throws Exception {
        consortium = new Consortium();
        consortium.setConsortiumName("Test Name");
        consortium.setConsortiumType("Test type");
        consortium = cnRepo.save(consortium);

        blockchain = new Blockchain();
        blockchain.setConsortium(consortium);
        blockchain.setIpList("1,2,3");
        blockchain = bcRepo.save(blockchain);
    }


    @Test
    public void findByIdTest() throws Exception {

        Optional<Blockchain> nbc = bcRepo.findById(blockchain.getId());
        Assert.assertTrue(nbc.isPresent());
        Assert.assertEquals(blockchain.getId(), nbc.get().getId());
    }

    @Test
    public void findByConsortiumTest() throws Exception {
        List<Blockchain> l = bcRepo.findAllByConsortium(consortium);
        Assert.assertEquals(1, l.size());
        Assert.assertEquals(blockchain.getId(), l.get(0).getId());
    }

    @Test
    public void getAsListTest() throws Exception {
        List<String> expected = ImmutableList.of("1", "2", "3");
        Assert.assertEquals(expected, blockchain.getIpAsList());
    }

    @Test
    public void updateListTest() {
        List<String> expected = ImmutableList.of("4", "3", "2", "1");
        blockchain.setIpAsList(expected);
        Blockchain bc = bcRepo.save(blockchain);
        Assert.assertEquals(expected, bc.getIpAsList());
    }

}

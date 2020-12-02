/*
 * Copyright (c) 2020 VMware, Inc. All rights reserved. VMware Confidential
 */

package com.vmware.blockchain.services.blockchains.helper;


import java.security.KeyPair;
import java.security.Security;

import org.bouncycastle.jce.provider.BouncyCastleProvider;
import org.junit.jupiter.api.AfterAll;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.Test;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.vmware.blockchain.services.blockchains.operator.helper.BlockchainOperatorCertificateGenerator;


/**
 * Class to generate keypair using {@link BouncyCastleProvider}.
 */
public class BlockchainOperatorCertificateGeneratorTest {

    private static Logger log = LoggerFactory.getLogger(BlockchainOperatorCertificateGeneratorTest.class);

    @BeforeAll
    static void setup() {
        Security.addProvider(new BouncyCastleProvider());
    }

    @AfterAll
    static void teardown() {
        Security.removeProvider("BC");
    }

    @Test
    public void generateKeys() {
        KeyPair keyPair = BlockchainOperatorCertificateGenerator.generateEcKeyPair();
        Assertions.assertTrue(BlockchainOperatorCertificateGenerator.returnPemString(keyPair.getPrivate())
                                      .contains("-----BEGIN EC PRIVATE KEY-----"));
        Assertions.assertTrue(BlockchainOperatorCertificateGenerator.returnPemString(keyPair.getPrivate())
                                      .contains("-----END EC PRIVATE KEY-----"));

        Assertions.assertTrue(BlockchainOperatorCertificateGenerator.returnPemString(keyPair.getPublic())
                                      .contains("-----BEGIN PUBLIC KEY-----"));
        Assertions.assertTrue(BlockchainOperatorCertificateGenerator.returnPemString(keyPair.getPublic())
                                      .contains("-----END PUBLIC KEY-----"));
    }
}

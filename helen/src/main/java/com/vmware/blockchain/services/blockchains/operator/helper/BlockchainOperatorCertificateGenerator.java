/*
 * Copyright (c) 2020 VMware, Inc. All rights reserved. VMware Confidential
 */

package com.vmware.blockchain.services.blockchains.operator.helper;


import java.io.IOException;
import java.io.StringWriter;
import java.security.InvalidAlgorithmParameterException;
import java.security.KeyPair;
import java.security.KeyPairGenerator;
import java.security.NoSuchAlgorithmException;
import java.security.SecureRandom;
import java.security.cert.X509Certificate;

import org.bouncycastle.jce.ECNamedCurveTable;
import org.bouncycastle.jce.provider.BouncyCastleProvider;
import org.bouncycastle.jce.spec.ECParameterSpec;
import org.bouncycastle.openssl.jcajce.JcaPEMWriter;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.http.HttpStatus;

import com.vmware.blockchain.common.HelenException;


/**
 * Class to generate keypair using {@link BouncyCastleProvider}.
 */
public class BlockchainOperatorCertificateGenerator {

    private static Logger log = LoggerFactory.getLogger(BlockchainOperatorCertificateGenerator.class);

    private static final SecureRandom random = new SecureRandom();

    /**
     * Generates {@link KeyPair}.
     * @return {@link KeyPair}
     */
    public static KeyPair generateEcKeyPair() {
        try {
            ECParameterSpec ecSpec = ECNamedCurveTable.getParameterSpec("secp256r1");
            KeyPairGenerator kpGen = KeyPairGenerator.getInstance("ECDSA");
            kpGen.initialize(ecSpec, random);
            return kpGen.generateKeyPair();
        } catch (NoSuchAlgorithmException | InvalidAlgorithmParameterException e) {
            log.error("Failed to generate key pair");
            e.printStackTrace();
            throw new HelenException(HttpStatus.INTERNAL_SERVER_ERROR, "Failed generating keys.");
        }
    }

    /**
     * returns PEM string format of keys/certs.
     * @param object : {@link java.security.Key} or {@link X509Certificate} object
     * @return PEM String
     */
    public static String returnPemString(Object object) {
        try {
            StringWriter writer = new StringWriter();
            JcaPEMWriter pemWriter = new JcaPEMWriter(writer);
            pemWriter.writeObject(object);
            pemWriter.close();
            String ecCert = writer.toString();
            writer.close();
            return ecCert;
        } catch (IOException e) {
            log.error("Creating string key failed");
            e.printStackTrace();
            throw new HelenException(HttpStatus.INTERNAL_SERVER_ERROR, "Failed parsing keys.");
        }

    }
}

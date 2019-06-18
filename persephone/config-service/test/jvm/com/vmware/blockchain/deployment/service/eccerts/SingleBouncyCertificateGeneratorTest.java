/*
 * Copyright (c) 2019 VMware, Inc. All rights reserved. VMware Confidential
 */

package com.vmware.blockchain.deployment.service.eccerts;

import static org.junit.jupiter.api.Assertions.assertDoesNotThrow;
import static org.junit.jupiter.api.Assertions.assertThrows;

import java.io.ByteArrayInputStream;
import java.io.InputStream;
import java.math.BigInteger;
import java.security.KeyFactory;
import java.security.PrivateKey;
import java.security.Security;
import java.security.SignatureException;
import java.security.cert.CertificateExpiredException;
import java.security.cert.CertificateFactory;
import java.security.cert.CertificateNotYetValidException;
import java.security.cert.X509Certificate;
import java.util.Calendar;
import java.util.Date;

import org.bouncycastle.jce.ECNamedCurveTable;
import org.bouncycastle.jce.provider.BouncyCastleProvider;
import org.bouncycastle.jce.spec.ECNamedCurveParameterSpec;
import org.bouncycastle.jce.spec.ECPrivateKeySpec;
import org.junit.jupiter.api.AfterAll;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.Test;

import com.vmware.blockchain.deployment.model.Identity;

/**
 * test for {@link SingleBouncyCertificateGenerator}.
 */
class SingleBouncyCertificateGeneratorTest {
    private static String filePath = "/tmp/tlsCerts";

    @BeforeAll
    static void setup() {
        Security.addProvider(new BouncyCastleProvider());
    }

    @AfterAll
    static void teardown() {
        Security.removeProvider("BC");
    }

    @Test
    public void testCertificateGenerationPositive() {
        assertDoesNotThrow(() -> {
            Identity ks = SingleBouncyCertificateGenerator
                    .generateIdentity("test", filePath, Identity.Type.TLS);

            assert ks.getCertificate().getComponentUrl().equalsIgnoreCase(filePath + "/tlsCerts.cert");
            assert ks.getKey().getComponentUrl().equalsIgnoreCase(filePath + "/pk.pem");

            CertificateFactory fact = CertificateFactory.getInstance("X.509");
            InputStream inputStream = new ByteArrayInputStream(ks.getCertificate().getComponent().getBytes());
            X509Certificate cert = (X509Certificate) fact.generateCertificate(inputStream);
            cert.checkValidity(new Date());
            cert.verify(cert.getPublicKey());
            assert cert.getPublicKey().getAlgorithm().equalsIgnoreCase("EC");
            assert cert.getSigAlgName().equalsIgnoreCase("SHA384WITHECDSA");

            byte[] keyBytes = ks.getKey().getComponent().getBytes();
            KeyFactory factory = KeyFactory.getInstance("ECDSA", "BC");
            ECNamedCurveParameterSpec spec = ECNamedCurveTable.getParameterSpec("secp384r1");
            ECPrivateKeySpec ecPrivateKeySpec = new ECPrivateKeySpec(new BigInteger(1, keyBytes), spec);
            PrivateKey privateKey = factory.generatePrivate(ecPrivateKeySpec);
            // TODO: Check private key wrt certificate (ECDSA is not straight forward)
            assert privateKey.getAlgorithm().equalsIgnoreCase("ECDSA");
        });
    }

    @Test
    public void testCertificateGenerationExpiredValidity() {
        assertThrows(CertificateExpiredException.class, () -> {
            Identity ks = SingleBouncyCertificateGenerator
                    .generateIdentity("test", filePath, Identity.Type.TLS);
            assert ks.getCertificate().getComponentUrl().equalsIgnoreCase(filePath + "/tlsCerts.cert");
            assert ks.getKey().getComponentUrl().equalsIgnoreCase(filePath + "/pk.pem");

            CertificateFactory fact = CertificateFactory.getInstance("X.509");
            InputStream inputStream = new ByteArrayInputStream(ks.getCertificate().getComponent().getBytes());
            X509Certificate cert = (X509Certificate) fact.generateCertificate(inputStream);

            Calendar cal = Calendar.getInstance();
            cal.setTime(new Date());
            cal.add(Calendar.DATE, 366);
            cert.checkValidity(cal.getTime());
        });
    }

    @Test
    public void testCertificateGenerationNotValidCertificate() {
        assertThrows(CertificateNotYetValidException.class, () -> {
            Identity ks = SingleBouncyCertificateGenerator
                    .generateIdentity("test", filePath, Identity.Type.TLS);
            assert ks.getCertificate().getComponentUrl().equalsIgnoreCase(filePath + "/tlsCerts.cert");
            assert ks.getKey().getComponentUrl().equalsIgnoreCase(filePath + "/pk.pem");

            CertificateFactory fact = CertificateFactory.getInstance("X.509");
            InputStream inputStream = new ByteArrayInputStream(ks.getCertificate().getComponent().getBytes());
            X509Certificate cert = (X509Certificate) fact.generateCertificate(inputStream);

            Calendar cal = Calendar.getInstance();
            cal.setTime(new Date());
            cal.add(Calendar.DATE, -1);
            cert.checkValidity(cal.getTime());
        });
    }

    @Test
    public void testCertificateGenerationUniqueCerts() {
        assertThrows(SignatureException.class, () -> {
            String fp1 = filePath + "/c1";
            String fp2 = filePath + "/c2";

            Identity ks1 = SingleBouncyCertificateGenerator.generateIdentity("test", fp1, Identity.Type.TLS);
            assert ks1.getKey().getComponentUrl().equalsIgnoreCase(fp1 + "/pk.pem");
            assert ks1.getCertificate().getComponentUrl().equalsIgnoreCase((fp1 + "/c1.cert"));

            Identity ks2 = SingleBouncyCertificateGenerator.generateIdentity("test", fp2, Identity.Type.TLS);
            assert ks2.getKey().getComponentUrl().equalsIgnoreCase(fp2 + "/pk.pem");
            assert ks2.getCertificate().getComponentUrl().equalsIgnoreCase(fp2 + "/c2.cert");

            CertificateFactory fact = CertificateFactory.getInstance("X.509");
            InputStream inputStream1 = new ByteArrayInputStream(ks1.getCertificate().getComponent().getBytes());
            X509Certificate cert1 = (X509Certificate) fact.generateCertificate(inputStream1);

            fact = CertificateFactory.getInstance("X.509");
            InputStream inputStream2 = new ByteArrayInputStream(ks2.getCertificate().getComponent().getBytes());
            X509Certificate cert2 = (X509Certificate) fact.generateCertificate(inputStream2);

            cert1.verify(cert2.getPublicKey());
            cert2.verify(cert1.getPublicKey());
        });
    }
}

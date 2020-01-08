/*
 * Copyright (c) 2019 VMware, Inc. All rights reserved. VMware Confidential
 */

package com.vmware.blockchain.configuration.eccerts;

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

import com.vmware.blockchain.deployment.v1.Identity;

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
                    .generateIdentity(filePath, "test-cn", "test-ou");

            assert ks.getCertificate().getUrl().equalsIgnoreCase(filePath + "/tlsCerts.cert");
            assert ks.getKey().getUrl().equalsIgnoreCase(filePath + "/pk.pem");

            CertificateFactory fact = CertificateFactory.getInstance("X.509");
            InputStream inputStream = new ByteArrayInputStream(ks.getCertificate().getBase64Value().getBytes());
            X509Certificate cert = (X509Certificate) fact.generateCertificate(inputStream);
            cert.checkValidity(new Date());
            cert.verify(cert.getPublicKey());
            assert cert.getPublicKey().getAlgorithm().equalsIgnoreCase("EC");
            assert cert.getSigAlgName().equalsIgnoreCase("SHA256WITHECDSA");
            assert cert.getIssuerDN().getName().equalsIgnoreCase(
                    "CN=test-cn, OU=test-ou, O=NA, L=NA, ST=NA, C=NA");

            byte[] keyBytes = ks.getKey().getBase64Value().getBytes();
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
                    .generateIdentity(filePath, "test-cn", "test-ou");
            assert ks.getCertificate().getUrl().equalsIgnoreCase(filePath + "/tlsCerts.cert");
            assert ks.getKey().getUrl().equalsIgnoreCase(filePath + "/pk.pem");

            CertificateFactory fact = CertificateFactory.getInstance("X.509");
            InputStream inputStream = new ByteArrayInputStream(ks.getCertificate().getBase64Value().getBytes());
            X509Certificate cert = (X509Certificate) fact.generateCertificate(inputStream);

            Calendar cal = Calendar.getInstance();
            cal.setTime(new Date());
            cal.add(Calendar.DATE, 1097);
            cert.checkValidity(cal.getTime());
        });
    }

    @Test
    public void testCertificateGenerationNotValidCertificate() {
        assertThrows(CertificateNotYetValidException.class, () -> {
            Identity ks = SingleBouncyCertificateGenerator
                    .generateIdentity(filePath, "test-cn", "test-ou");
            assert ks.getCertificate().getUrl().equalsIgnoreCase(filePath + "/tlsCerts.cert");
            assert ks.getKey().getUrl().equalsIgnoreCase(filePath + "/pk.pem");

            CertificateFactory fact = CertificateFactory.getInstance("X.509");
            InputStream inputStream = new ByteArrayInputStream(ks.getCertificate().getBase64Value().getBytes());
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

            Identity ks1 = SingleBouncyCertificateGenerator.generateIdentity(fp1, "test-cn", "test-ou");
            assert ks1.getKey().getUrl().equalsIgnoreCase(fp1 + "/pk.pem");
            assert ks1.getCertificate().getUrl().equalsIgnoreCase((fp1 + "/c1.cert"));

            Identity ks2 = SingleBouncyCertificateGenerator.generateIdentity(fp2, "test-cn", "test-ou");
            assert ks2.getKey().getUrl().equalsIgnoreCase(fp2 + "/pk.pem");
            assert ks2.getCertificate().getUrl().equalsIgnoreCase(fp2 + "/c2.cert");

            CertificateFactory fact = CertificateFactory.getInstance("X.509");
            InputStream inputStream1 = new ByteArrayInputStream(ks1.getCertificate().getBase64Value().getBytes());
            X509Certificate cert1 = (X509Certificate) fact.generateCertificate(inputStream1);

            fact = CertificateFactory.getInstance("X.509");
            InputStream inputStream2 = new ByteArrayInputStream(ks2.getCertificate().getBase64Value().getBytes());
            X509Certificate cert2 = (X509Certificate) fact.generateCertificate(inputStream2);

            cert1.verify(cert2.getPublicKey());
            cert2.verify(cert1.getPublicKey());
        });
    }
}

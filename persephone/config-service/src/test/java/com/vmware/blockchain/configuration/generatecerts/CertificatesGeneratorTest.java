/*
 * Copyright (c) 2019 VMware, Inc. All rights reserved. VMware Confidential
 */

package com.vmware.blockchain.configuration.generatecerts;

import static org.junit.jupiter.api.Assertions.assertDoesNotThrow;

import java.io.ByteArrayInputStream;
import java.io.InputStream;
import java.security.Security;
import java.security.cert.CertificateException;
import java.security.cert.CertificateFactory;
import java.security.cert.X509Certificate;
import java.util.List;
import java.util.stream.Collectors;
import java.util.stream.IntStream;

import org.bouncycastle.jce.provider.BouncyCastleProvider;
import org.junit.jupiter.api.AfterAll;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.Test;

import com.vmware.blockchain.configuration.eccerts.ConcordEcCertificatesGenerator;
import com.vmware.blockchain.configuration.eccerts.TrsTrcTlsSingleCertificateGenerator;
import com.vmware.blockchain.deployment.v1.ConcordComponent.ServiceType;
import com.vmware.blockchain.deployment.v1.Identity;
import com.vmware.blockchain.deployment.v1.IdentityFactors;

/**
 * test for {@link CertificatesGenerator}.
 */
class CertificatesGeneratorTest {

    @BeforeAll
    static void setup() {
        Security.addProvider(new BouncyCastleProvider());
    }

    @AfterAll
    static void teardown() {
        Security.removeProvider("BC");
    }

    @Test
    void testGenerateConcordSelfSignedCertificates() {
        assertDoesNotThrow(() -> {
            CertificatesGenerator certGen = new ConcordEcCertificatesGenerator();
            List<Identity> certList = certGen.generateSelfSignedCertificates(3,
                    ServiceType.CONCORD);

            assert certList.size() == 6;

            List<Identity> serverList = certList.stream().limit(3).collect(Collectors.toList());
            List<Identity> clientList = certList.subList(certList.size() - 3, certList.size());

            for (int index = 0; index < serverList.size(); index++) {
                Identity server = serverList.get(index);

                assert server.getCertificate().getUrl().equalsIgnoreCase(
                        String.format("%s/%s/server/server.cert",
                                certGen.FILE_PREFIX + certGen.CONCORD_TLS_SECURITY_IDENTITY_PATH, index));
                assert server.getKey().getUrl().equalsIgnoreCase(String.format("%s/%s/server/pk.pem",
                        certGen.FILE_PREFIX + certGen.CONCORD_TLS_SECURITY_IDENTITY_PATH, index));

                testCertificate(server, "node" + index + "ser", String.valueOf(index), null);
            }

            for (int index = 0; index < clientList.size(); index++) {
                Identity client = clientList.get(index);

                assert client.getCertificate().getUrl().equalsIgnoreCase(
                        String.format("%s/%s/client/client.cert",
                                certGen.FILE_PREFIX + certGen.CONCORD_TLS_SECURITY_IDENTITY_PATH, index));
                assert client.getKey().getUrl()
                        .equalsIgnoreCase(String.format("%s/%s/client/pk.pem",
                                certGen.FILE_PREFIX + certGen.CONCORD_TLS_SECURITY_IDENTITY_PATH, index));

                testCertificate(client, "node" + index + "cli", String.valueOf(index), null);
            }
        });
    }

    @Test
    void testGenerateEthRpcSelfSignedCertificates() {
        assertDoesNotThrow(() -> {
            int numCerts = 3;
            CertificatesGenerator certGen = new ConcordEcCertificatesGenerator();
            List<String> paths = IntStream.range(0, numCerts).boxed()
                    .map(entry -> String.join("/",
                            certGen.FILE_PREFIX + certGen.CONCORD_ETHRPC_SECURITY_IDENTITY_PATH,
                            String.valueOf(entry)))
                    .collect(Collectors.toList());
            List<Identity> certList = certGen.generateSelfSignedCertificates(numCerts,
                    ServiceType.ETHEREUM_API);

            assert certList.size() == numCerts;

            for (int index = 0; index < certList.size(); index++) {
                Identity identity = certList.get(index);

                assert identity.getCertificate().getUrl().equalsIgnoreCase(
                        String.format("%s/%s.cert", paths.get(index), index));
                assert identity.getKey().getUrl().equalsIgnoreCase(
                        String.format("%s/pk.pem", paths.get(index)));

                testCertificate(identity, "node" + index, String.valueOf(index), null);
            }
        });
    }

    @Test
    void testTrsTrcTlsCertificates() {
        assertDoesNotThrow(() -> {
            String cn = "1.2.3.4";
            String ou = "myOU";
            CertificatesGenerator certGen = new TrsTrcTlsSingleCertificateGenerator();
            var actual = certGen.generateSelfSignedCertificates(1, ServiceType.CONCORD, cn, ou);
            assert (actual.size() == 1);
            Identity identity = actual.get(0);
            testCertificate(identity, cn, ou, cn);
            assert (identity.getCertificate().getUrl().equals(CertificatesGenerator.FILE_PREFIX
                    + CertificatesGenerator.TRS_TLS_IDENTITY_PATH + "/server.cert"));
            assert (identity.getKey().getUrl().equals(CertificatesGenerator.FILE_PREFIX
                    + CertificatesGenerator.TRS_TLS_IDENTITY_PATH + "/pk.pem"));

            actual = certGen.generateSelfSignedCertificates(1, ServiceType.DAML_LEDGER_API, cn, ou);
            assert (actual.size() == 1);
            identity = actual.get(0);
            testCertificate(identity, cn, ou, cn);
            assert (identity.getCertificate().getUrl().equals(CertificatesGenerator.FILE_PREFIX
                    + CertificatesGenerator.TRC_TLS_IDENTITY_PATH + "/client.cert"));
            assert (identity.getKey().getUrl().equals(CertificatesGenerator.FILE_PREFIX
                    + CertificatesGenerator.TRC_TLS_IDENTITY_PATH + "/pk.pem"));
        });
    }

    @Test
    void testIdentityFactors() {
        CertificatesGenerator certGen = new ConcordEcCertificatesGenerator();
        testIdentityFactor(certGen.getIdentityFactor());

        certGen = new TrsTrcTlsSingleCertificateGenerator();
        testIdentityFactor(certGen.getIdentityFactor());
    }

    private void testCertificate(Identity identity, String cn, String ou, String san) throws CertificateException {
        CertificateFactory fact = CertificateFactory.getInstance("X.509");
        InputStream inputStream = new ByteArrayInputStream(identity.getCertificate()
                .getBase64Value().getBytes());
        X509Certificate cert = (X509Certificate) fact.generateCertificate(inputStream);
        assert cert.getIssuerDN().getName().equalsIgnoreCase(
                "CN=" + cn + ", OU=" + ou + ", O=NA, L=NA, ST=NA, C=NA");
        if (san != null) {
            assert cert.getSubjectAlternativeNames().size() == 1;
            cert.getSubjectAlternativeNames().forEach(name -> {
                assert name.get(1).equals(san);
            });
        }
    }

    private void testIdentityFactor(IdentityFactors identityFactor) {
        assert identityFactor.getAlgorithm().equalsIgnoreCase("ECDSA");
        assert identityFactor.getCurve().equalsIgnoreCase("secp384r1");
        assert identityFactor.getSigningAlgorithm().equalsIgnoreCase("SHA384WITHECDSA");
    }

}

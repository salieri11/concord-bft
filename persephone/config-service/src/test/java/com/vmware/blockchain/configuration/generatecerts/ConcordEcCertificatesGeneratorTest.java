/*
 * Copyright (c) 2019 VMware, Inc. All rights reserved. VMware Confidential
 */

package com.vmware.blockchain.configuration.generatecerts;

import static org.junit.jupiter.api.Assertions.assertDoesNotThrow;

import java.io.ByteArrayInputStream;
import java.io.InputStream;
import java.security.Security;
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
import com.vmware.blockchain.deployment.v1.ConcordComponent.ServiceType;
import com.vmware.blockchain.deployment.v1.Identity;

/**
 * test for {@link CertificatesGenerator}.
 */
class ConcordEcCertificatesGeneratorTest {

    @BeforeAll
    static void setup() {
        Security.addProvider(new BouncyCastleProvider());
    }

    @AfterAll
    static void teardown() {
        Security.removeProvider("BC");
    }

    @Test
    void testgenerateTlsSelfSignedCertificates() {
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
                        String.format("%s/%s/server/server.cert", certGen.CONCORD_TLS_SECURITY_IDENTITY_PATH, index));
                assert server.getKey().getUrl().equalsIgnoreCase(String.format("%s/%s/server/pk.pem",
                        certGen.CONCORD_TLS_SECURITY_IDENTITY_PATH, index));

                CertificateFactory fact = CertificateFactory.getInstance("X.509");
                InputStream inputStream = new ByteArrayInputStream(server.getCertificate().getBase64Value().getBytes());
                X509Certificate cert = (X509Certificate) fact.generateCertificate(inputStream);
                assert cert.getIssuerDN().getName().equalsIgnoreCase(
                        "CN=node" + index + "ser, OU=" + index + ", O=NA, L=NA, ST=NA, C=NA");
            }

            for (int index = 0; index < clientList.size(); index++) {
                Identity client = clientList.get(index);

                assert client.getCertificate().getUrl().equalsIgnoreCase(
                        String.format("%s/%s/client/client.cert", certGen.CONCORD_TLS_SECURITY_IDENTITY_PATH, index));
                assert client.getKey().getUrl()
                        .equalsIgnoreCase(String.format("%s/%s/client/pk.pem",
                                certGen.CONCORD_TLS_SECURITY_IDENTITY_PATH, index));

                CertificateFactory fact = CertificateFactory.getInstance("X.509");
                InputStream inputStream = new ByteArrayInputStream(client.getCertificate().getBase64Value().getBytes());
                X509Certificate cert = (X509Certificate) fact.generateCertificate(inputStream);
                assert cert.getIssuerDN().getName().equalsIgnoreCase(
                        "CN=node" + index + "cli, OU=" + index + ", O=NA, L=NA, ST=NA, C=NA");
            }
        });
    }

    @Test
    void testgenerateEthRpcSelfSignedCertificates() {
        assertDoesNotThrow(() -> {
            int numCerts = 3;
            CertificatesGenerator certGen = new ConcordEcCertificatesGenerator();
            List<String> paths = IntStream.range(0, numCerts).boxed()
                    .map(entry -> String.join("/", certGen.CONCORD_ETHRPC_SECURITY_IDENTITY_PATH,
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

                CertificateFactory fact = CertificateFactory.getInstance("X.509");
                InputStream inputStream = new ByteArrayInputStream(identity.getCertificate()
                        .getBase64Value().getBytes());
                X509Certificate cert = (X509Certificate) fact.generateCertificate(inputStream);
                assert cert.getIssuerDN().getName().equalsIgnoreCase(
                        "CN=node" + index + ", OU=" + index + ", O=NA, L=NA, ST=NA, C=NA");
            }
        });
    }

    @Test
    void testIdentityFactors() {
        CertificatesGenerator certGen = new ConcordEcCertificatesGenerator();
        var identityFactor = certGen.getIdentityFactor();

        assert identityFactor.getAlgorithm().equalsIgnoreCase("ECDSA");
        assert identityFactor.getCurve().equalsIgnoreCase("secp384r1");
        assert identityFactor.getSigningAlgorithm().equalsIgnoreCase("SHA384WITHECDSA");
    }

}

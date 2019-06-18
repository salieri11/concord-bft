/*
 * Copyright (c) 2019 VMware, Inc. All rights reserved. VMware Confidential
 */

package com.vmware.blockchain.deployment.service.generatecerts;

import static org.junit.jupiter.api.Assertions.assertDoesNotThrow;

import java.io.ByteArrayInputStream;
import java.io.InputStream;
import java.security.Security;
import java.security.cert.CertificateFactory;
import java.security.cert.X509Certificate;
import java.util.Arrays;
import java.util.List;
import java.util.stream.Collectors;

import org.bouncycastle.jce.provider.BouncyCastleProvider;
import org.junit.jupiter.api.AfterAll;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.Test;

import com.vmware.blockchain.deployment.model.Identity;
import com.vmware.blockchain.deployment.service.eccerts.ConcordCertificatesGenerator;

/**
 * test for {@link CertificatesGenerator}.
 */
class ConcordCertificatesGeneratorTest {

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
    void testgenerateTlsSelfSignedCertificates() {
        assertDoesNotThrow(() -> {
            CertificatesGenerator certGen = new ConcordCertificatesGenerator();
            List<Identity> certList = certGen.generateTlsSelfSignedCertificates(3, filePath);

            assert certList.size() == 6;

            List<Identity> serverList = certList.stream().limit(3).collect(Collectors.toList());
            List<Identity> clientList = certList.subList(certList.size() - 3, certList.size());

            for (int index = 0; index < serverList.size(); index++) {
                Identity server = serverList.get(index);

                assert server.getType().equals(Identity.Type.TLS);
                assert server.getCertificate().getComponentUrl().equalsIgnoreCase(
                        String.format("%s/%s/server/server.cert", filePath, index));
                assert server.getKey().getComponentUrl().equalsIgnoreCase(String.format("%s/%s/server/pk.pem",
                        filePath, index));

                CertificateFactory fact = CertificateFactory.getInstance("X.509");
                InputStream inputStream = new ByteArrayInputStream(server.getCertificate().getComponent().getBytes());
                X509Certificate cert = (X509Certificate) fact.generateCertificate(inputStream);
                assert cert.getIssuerDN().getName().equalsIgnoreCase("CN=node" + index + "ser");
            }

            for (int index = 0; index < clientList.size(); index++) {
                Identity client = clientList.get(index);

                assert client.getType().equals(Identity.Type.TLS);
                assert client.getCertificate().getComponentUrl().equalsIgnoreCase(
                        String.format("%s/%s/client/client.cert", filePath, index));
                assert client.getKey().getComponentUrl()
                        .equalsIgnoreCase(String.format("%s/%s/client/pk.pem", filePath, index));

                CertificateFactory fact = CertificateFactory.getInstance("X.509");
                InputStream inputStream = new ByteArrayInputStream(client.getCertificate().getComponent().getBytes());
                X509Certificate cert = (X509Certificate) fact.generateCertificate(inputStream);
                assert cert.getIssuerDN().getName().equalsIgnoreCase("CN=node" + index + "cli");
            }
        });
    }

    @Test
    void testgenerateEthRpcSelfSignedCertificates() {
        assertDoesNotThrow(() -> {
            List<String> paths = Arrays.asList(filePath + "/node0", filePath + "/node1", filePath + "/node2");
            CertificatesGenerator certGen = new ConcordCertificatesGenerator();
            List<Identity> certList = certGen.generateEthRpcSelfSignedCertificates(paths);

            assert certList.size() == 3;

            for (int index = 0; index < certList.size(); index++) {
                Identity identity = certList.get(index);

                assert identity.getType().equals(Identity.Type.ETHRPC);
                assert identity.getCertificate().getComponentUrl().equalsIgnoreCase(
                        String.format("%s/node%s.cert", paths.get(index), index));
                assert identity.getKey().getComponentUrl().equalsIgnoreCase(
                        String.format("%s/pk.pem", paths.get(index)));

                CertificateFactory fact = CertificateFactory.getInstance("X.509");
                InputStream inputStream = new ByteArrayInputStream(identity.getCertificate().getComponent().getBytes());
                X509Certificate cert = (X509Certificate) fact.generateCertificate(inputStream);
                assert cert.getIssuerDN().getName().equalsIgnoreCase("CN=node" + index);
            }
        });
    }

}

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
import java.util.List;
import java.util.stream.Collectors;
import java.util.stream.IntStream;

import org.bouncycastle.jce.provider.BouncyCastleProvider;
import org.junit.jupiter.api.AfterAll;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.Test;

import com.vmware.blockchain.deployment.model.ConfigurationServiceType;
import com.vmware.blockchain.deployment.model.Identity;
import com.vmware.blockchain.deployment.service.eccerts.ConcordCertificatesGenerator;
import com.vmware.blockchain.deployment.service.util.Constants;

/**
 * test for {@link CertificatesGenerator}.
 */
class ConcordCertificatesGeneratorTest {

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
            List<Identity> certList = certGen.generateSelfSignedCertificates(3,
                    ConfigurationServiceType.Type.TLS);

            assert certList.size() == 6;

            List<Identity> serverList = certList.stream().limit(3).collect(Collectors.toList());
            List<Identity> clientList = certList.subList(certList.size() - 3, certList.size());

            for (int index = 0; index < serverList.size(); index++) {
                Identity server = serverList.get(index);

                assert server.getCertificate().getComponentUrl().equalsIgnoreCase(
                        String.format("%s/%s/server/server.cert", Constants.TLS_IDENTITY_PATH, index));
                assert server.getKey().getComponentUrl().equalsIgnoreCase(String.format("%s/%s/server/pk.pem",
                        Constants.TLS_IDENTITY_PATH, index));

                CertificateFactory fact = CertificateFactory.getInstance("X.509");
                InputStream inputStream = new ByteArrayInputStream(server.getCertificate().getComponent().getBytes());
                X509Certificate cert = (X509Certificate) fact.generateCertificate(inputStream);
                assert cert.getIssuerDN().getName().equalsIgnoreCase("CN=node" + index + "ser");
            }

            for (int index = 0; index < clientList.size(); index++) {
                Identity client = clientList.get(index);

                assert client.getCertificate().getComponentUrl().equalsIgnoreCase(
                        String.format("%s/%s/client/client.cert", Constants.TLS_IDENTITY_PATH, index));
                assert client.getKey().getComponentUrl()
                        .equalsIgnoreCase(String.format("%s/%s/client/pk.pem", Constants.TLS_IDENTITY_PATH, index));

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
            int numCerts = 3;
            List<String> paths = IntStream.range(0, numCerts).boxed()
                    .map(entry -> String.join("/", Constants.ETHRPC_IDENTITY_PATH,
                            String.valueOf(entry)))
                    .collect(Collectors.toList());
            CertificatesGenerator certGen = new ConcordCertificatesGenerator();
            List<Identity> certList = certGen.generateSelfSignedCertificates(numCerts,
                    ConfigurationServiceType.Type.ETHRPC);

            assert certList.size() == numCerts;

            for (int index = 0; index < certList.size(); index++) {
                Identity identity = certList.get(index);

                assert identity.getCertificate().getComponentUrl().equalsIgnoreCase(
                        String.format("%s/%s.cert", paths.get(index), index));
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

/*
 * Copyright (c) 2020 VMware, Inc. All rights reserved. VMware Confidential
 */

package com.vmware.blockchain.server;

import java.security.Security;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.bouncycastle.jce.provider.BouncyCastleProvider;
import org.junit.jupiter.api.AfterAll;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.Test;

import com.vmware.blockchain.configuration.eccerts.ConcordEcCertificatesGenerator;
import com.vmware.blockchain.configuration.generatecerts.CertificatesGenerator;
import com.vmware.blockchain.deployment.v1.ConcordComponent;
import com.vmware.blockchain.deployment.v1.Identity;
import com.vmware.blockchain.deployment.v1.IdentityComponent;

/**
 * Unit test for config service helper methods.
 */
public class ConfigurationServiceHelperTest {

    @BeforeAll
    static void setup() {
        Security.addProvider(new BouncyCastleProvider());
    }

    @AfterAll
    static void teardown() {
        Security.removeProvider("BC");
    }

    ConfigurationServiceHelper configurationServiceHelper = new ConfigurationServiceHelper(
            "fakePath", "fakePath", "fakePath", "fakePath");

    @Test
    void testConvertToBftTlsNodeIdentities() {
        Map<String, List<IdentityComponent>> input = new HashMap<>();
        List<IdentityComponent> identities1 = new ArrayList<>();

        identities1.add(IdentityComponent.newBuilder()
                .setType(IdentityComponent.Type.UNKNOWN)
                .setBase64Value("value1")
                .setUrl("not valid")
                .build());
        identities1.add(IdentityComponent.newBuilder()
                .setType(IdentityComponent.Type.UNKNOWN)
                .setBase64Value("value2")
                .setUrl(CertificatesGenerator.CONCORD_TLS_SECURITY_IDENTITY_PATH)
                .build());

        input.put("id1", identities1);

        List<IdentityComponent> identities2 = new ArrayList<>();
        identities2.add(IdentityComponent.newBuilder()
                .setType(IdentityComponent.Type.UNKNOWN)
                .setBase64Value("value3")
                .setUrl(CertificatesGenerator.CONCORD_TLS_SECURITY_IDENTITY_PATH)
                .build());
        identities2.add(IdentityComponent.newBuilder()
                .setType(IdentityComponent.Type.UNKNOWN)
                .setBase64Value("value4")
                .setUrl("not valid")
                .build());

        input.put("id2", identities2);

        var actual = configurationServiceHelper.convertToBftTlsNodeIdentities(input);

        actual.forEach((key, value) -> {
            if (key.equalsIgnoreCase("id1")) {
                value.forEach(ident -> {
                    if (ident.getBase64Value().equalsIgnoreCase("value2")
                            || ident.getBase64Value().equalsIgnoreCase("value3")) {
                        Assertions.assertEquals(ident.getUrl(),
                                CertificatesGenerator.BFT_CLIENT_TLS_SECURITY_IDENTITY_PATH);
                    } else {
                        Assertions.assertNotEquals(ident.getUrl(),
                                CertificatesGenerator.BFT_CLIENT_TLS_SECURITY_IDENTITY_PATH);
                    }

                });
            }
        });
    }

    @Test
    void testBuildTlsIdentity() {
        var certGen = new ConcordEcCertificatesGenerator();

        List<String> nodeIds = Arrays.asList("node1", "node2", "node3", "node4");
        int numCerts = 20;

        List<Identity> identities = certGen.generateSelfSignedCertificates(numCerts,
                ConcordComponent.ServiceType.CONCORD);

        Map<Integer, List<Integer>> principals = new HashMap<>();
        principals.put(0, Arrays.asList(0, 5));
        principals.put(1, Arrays.asList(1, 7));
        principals.put(2, Arrays.asList(2, 9));
        principals.put(3, Arrays.asList(3, 11));

        var actual = configurationServiceHelper.buildTlsIdentity(nodeIds, identities, principals, numCerts);

        Assertions.assertFalse(actual.isEmpty());

        actual.forEach((key, value) -> {
            value.forEach(identity -> {
                switch (key) {
                    case "node1":
                        if (identity.getType().equals(IdentityComponent.Type.KEY)) {
                            Assertions.assertTrue(identity.getUrl().contains("/0/")
                                    || identity.getUrl().contains("/5/"));
                        }
                        break;
                    case "node2":
                        if (identity.getType().equals(IdentityComponent.Type.KEY)) {
                            Assertions.assertTrue(identity.getUrl().contains("/1/")
                                    || identity.getUrl().contains("/7/"));
                        }
                        break;
                    case "node3":
                        if (identity.getType().equals(IdentityComponent.Type.KEY)) {
                            Assertions.assertTrue(identity.getUrl().contains("/2/")
                                    || identity.getUrl().contains("/9/"));
                        }
                        break;
                    case "node4":
                        if (identity.getType().equals(IdentityComponent.Type.KEY)) {
                            Assertions.assertTrue(identity.getUrl().contains("/3/")
                                    || identity.getUrl().contains("/11/"));
                        }
                        break;
                    default:
                        Assertions.assertFalse(identity.getType().equals(IdentityComponent.Type.KEY));
                }
            });
        });

    }
}

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
public class ConfigurationServiceUtilTest {

    static int numConcordCerts = 20;
    static int numBftCerts = 35;
    ConcordEcCertificatesGenerator certGen = new ConcordEcCertificatesGenerator();
    static Map<Integer, List<Integer>> concordPrincipals = new HashMap<>();
    static Map<Integer, List<Integer>> bftPrincipals = new HashMap<>();

    @BeforeAll
    static void setup() {
        Security.addProvider(new BouncyCastleProvider());

        concordPrincipals.put(0, Arrays.asList(0, 5));
        concordPrincipals.put(1, Arrays.asList(1, 7));
        concordPrincipals.put(2, Arrays.asList(2, 9));
        concordPrincipals.put(3, Arrays.asList(3, 11));

        bftPrincipals.put(0, Arrays.asList(21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31, 32, 33, 34, 35));
    }

    @AfterAll
    static void teardown() {
        Security.removeProvider("BC");
    }

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

        var actual = ConfigurationServiceUtil.convertToBftTlsNodeIdentities(input);

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
    void testGetTlsNodeIdentitiesConcord() {
        List<String> nodeIds = Arrays.asList("node1", "node2", "node3", "node4", "participant0");

        Map<String, List<IdentityComponent>> actual = ConfigurationServiceUtil.getTlsNodeIdentities(numConcordCerts,
                concordPrincipals,
                numBftCerts,
                bftPrincipals,
                certGen,
                nodeIds,
                false);

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

    @Test
    void testGetTlsNodeIdentitiesBftClient() {
        List<String> nodeIds = Arrays.asList("node1", "node2", "node3", "node4", "participant0");

        Map<String, List<IdentityComponent>> actual = ConfigurationServiceUtil.getTlsNodeIdentities(numConcordCerts,
                concordPrincipals,
                numBftCerts,
                bftPrincipals,
                certGen,
                nodeIds,
                true);

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
                    case "participant0":
                        if (identity.getType().equals(IdentityComponent.Type.KEY)) {
                            Assertions.assertTrue(identity.getUrl().contains("/4/")
                                    || identity.getUrl().contains("/21/")
                                    || identity.getUrl().contains("/22/")
                                    || identity.getUrl().contains("/23/")
                                    || identity.getUrl().contains("/24/")
                                    || identity.getUrl().contains("/25/")
                                    || identity.getUrl().contains("/26/")
                                    || identity.getUrl().contains("/27/")
                                    || identity.getUrl().contains("/28/")
                                    || identity.getUrl().contains("/29/")
                                    || identity.getUrl().contains("/30/")
                                    || identity.getUrl().contains("/31/")
                                    || identity.getUrl().contains("/32/")
                                    || identity.getUrl().contains("/33/")
                                    || identity.getUrl().contains("/34/")
                                    || identity.getUrl().contains("/35/"));
                        }
                        break;
                    default:
                        Assertions.assertFalse(identity.getType().equals(IdentityComponent.Type.KEY));
                }
            });
        });
    }

    @Test
    void testBuildTlsIdentity() {
        List<String> nodeIds = Arrays.asList("node1", "node2", "node3", "node4");
        List<Identity> identities = certGen.generateSelfSignedCertificates(numConcordCerts,
                ConcordComponent.ServiceType.CONCORD);

        var actual = ConfigurationServiceUtil.buildTlsIdentity(nodeIds, identities, concordPrincipals, numConcordCerts);

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

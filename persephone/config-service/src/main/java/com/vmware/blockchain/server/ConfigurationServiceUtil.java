/*
 * Copyright (c) 2019 VMware, Inc. All rights reserved. VMware Confidential
 */

package com.vmware.blockchain.server;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.UUID;
import java.util.stream.Collectors;
import java.util.stream.IntStream;

import com.vmware.blockchain.configuration.eccerts.ConcordEcCertificatesGenerator;
import com.vmware.blockchain.configuration.generatecerts.CertificatesGenerator;
import com.vmware.blockchain.deployment.v1.ConcordComponent;
import com.vmware.blockchain.deployment.v1.ConfigurationSessionIdentifier;
import com.vmware.blockchain.deployment.v1.Identity;
import com.vmware.blockchain.deployment.v1.IdentityComponent;

import lombok.extern.slf4j.Slf4j;

/**
 * Implementation of ConfigurationService server.
 */
@Slf4j
public class ConfigurationServiceUtil {

    /**
     * Generate a new {@link ConfigurationSessionIdentifier}.
     *
     * @return
     *   a new {@link ConfigurationSessionIdentifier} instance.
     */

    static ConfigurationSessionIdentifier newSessionUId() {
        return ConfigurationSessionIdentifier.newBuilder().setId(UUID.randomUUID().toString()).build();
    }

    /**
     * Get tls node identities.
     * TODO: better refactoring when deprecated code cleaned up
     */
    static Map<String, List<IdentityComponent>> getTlsNodeIdentities(int concordNumPrincipals,
                                                              Map<Integer, List<Integer>> concordNodePrincipals,
                                                              int bftClientNumPrincipals,
                                                              Map<Integer, List<Integer>> bftClientNodePrincipals,
                                                              ConcordEcCertificatesGenerator certGen,
                                                              List<String> nodeIdList,
                                                              boolean isBftEnabled) {

        int numPrincipals = concordNumPrincipals + 1;
        Map<Integer, List<Integer>> nodePrincipal = new HashMap<>(concordNodePrincipals);

        if (isBftEnabled) {
            numPrincipals = bftClientNumPrincipals + 1;
            bftClientNodePrincipals.forEach((key, value) ->
                    nodePrincipal.put(concordNodePrincipals.size() + key, value));
        }

        List<Identity> tlsIdentityList =
                certGen.generateSelfSignedCertificates(numPrincipals,
                        ConcordComponent.ServiceType.CONCORD);

        Map<String, List<IdentityComponent>> tlsNodeIdentities = buildTlsIdentity(nodeIdList,
                tlsIdentityList,
                nodePrincipal,
                numPrincipals);
        return tlsNodeIdentities;
    }

    /**
     * Filter tls identities based on nodes and principal ids.
     */
    static Map<String, List<IdentityComponent>> buildTlsIdentity(List<String> nodeIds,
                                                          List<Identity> identities,
                                                          Map<Integer, List<Integer>> principals,
                                                          int numCerts) {

        Map<String, List<IdentityComponent>> result = new HashMap<>();

        // TODO: May remove logic once principals are available
        if (principals.size() == 0) {
            IntStream.range(0, nodeIds.size()).forEach(node -> {
                List<IdentityComponent> identityComponents = new ArrayList<>();
                identities.forEach(identity -> {
                    identityComponents.add(identity.getCertificate());
                    identityComponents.add(identity.getKey());
                });
                result.put(nodeIds.get(node), identityComponents);
            });
            return result;
        }

        for (int node : principals.keySet()) {
            List<IdentityComponent> nodeIdentities = new ArrayList<>();

            List<Integer> notPrincipal = IntStream.range(0, numCerts)
                    .boxed().collect(Collectors.toList());
            notPrincipal.removeAll(principals.get(node));

            List<Identity> serverList = new ArrayList<>(identities.subList(0, identities.size() / 2));
            List<Identity> clientList = new ArrayList<>(identities.subList(identities.size() / 2, identities.size()));

            notPrincipal.forEach(entry -> {
                nodeIdentities.add(serverList.get(entry).getCertificate());
                nodeIdentities.add(clientList.get(entry).getCertificate());
            });

            // add self keys
            nodeIdentities.add(serverList.get(node).getKey());
            nodeIdentities.add(clientList.get(node).getKey());

            principals.get(node).forEach(entry -> {
                nodeIdentities.add(serverList.get(entry).getCertificate());
                nodeIdentities.add(serverList.get(entry).getKey());
                nodeIdentities.add(clientList.get(entry).getCertificate());
                nodeIdentities.add(clientList.get(entry).getKey());
            });
            result.putIfAbsent(nodeIds.get(node), nodeIdentities);
        }

        log.info("Filtered tls identities based on nodes and principal ids.");
        return result;
    }

    /**
     * transform concord node identities to bft node identities.
     */
    static Map<String, List<IdentityComponent>> convertToBftTlsNodeIdentities(Map<String, List<IdentityComponent>>
                                                                               tlsNodeIdentities) {
        Map<String, List<IdentityComponent>> bftIdentityComponents = new HashMap<>();

        tlsNodeIdentities.forEach((key, value) -> {
            List<IdentityComponent> bftIdentities = new ArrayList<>();
            value.forEach(val -> {
                String newUrl = val.getUrl().replace(
                        CertificatesGenerator.CONCORD_TLS_SECURITY_IDENTITY_PATH,
                        CertificatesGenerator.BFT_CLIENT_TLS_SECURITY_IDENTITY_PATH);
                IdentityComponent ident = IdentityComponent.newBuilder()
                        .setType(val.getType())
                        .setBase64Value(val.getBase64Value())
                        .setUrl(newUrl)
                        .build();
                bftIdentities.add(ident);
            });
            bftIdentityComponents.put(key, bftIdentities);
        });

        return bftIdentityComponents;
    }
}

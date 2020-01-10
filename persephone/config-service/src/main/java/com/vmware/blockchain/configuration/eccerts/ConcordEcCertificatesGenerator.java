/*
 * Copyright (c) 2019 VMware, Inc. All rights reserved. VMware Confidential
 */

package com.vmware.blockchain.configuration.eccerts;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.concurrent.CompletableFuture;
import java.util.stream.Collectors;
import java.util.stream.IntStream;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.vmware.blockchain.deployment.v1.ConcordComponent.ServiceType;
import com.vmware.blockchain.deployment.v1.Identity;
import com.vmware.blockchain.deployment.v1.IdentityFactors;

/**
 * This class is a bouncycastle implementation of getting ssl certs and keypair.
 */
public class ConcordEcCertificatesGenerator implements
                                            com.vmware.blockchain.configuration.generatecerts.CertificatesGenerator {

    private static Logger log = LoggerFactory.getLogger(ConcordEcCertificatesGenerator.class);

    @Override
    public List<Identity> generateSelfSignedCertificates(int numCerts, ServiceType type) {

        List<Map.Entry<Integer, String>> certSubjectVars;
        List<String> directoryList;

        if (type.equals(ServiceType.CONCORD)) {
            directoryList = getCertDirectories(numCerts, CONCORD_TLS_SECURITY_IDENTITY_PATH);

            certSubjectVars = IntStream.range(0, numCerts).boxed()
                    .map(entry -> Map.entry(entry, "node" + entry + "ser"))
                    .collect(Collectors.toList());
            certSubjectVars.addAll(IntStream.range(0, numCerts).boxed()
                    .map(entry -> Map.entry(entry, "node" + entry + "cli"))
                    .collect(Collectors.toList()));
        } else if (type.equals(ServiceType.ETHEREUM_API)) {
            directoryList = new ArrayList<>(getCertDirectories(numCerts, CONCORD_ETHRPC_SECURITY_IDENTITY_PATH));
            certSubjectVars = IntStream.range(0, numCerts).boxed()
                    .map(entry -> Map.entry(entry, "node" + entry))
                    .collect(Collectors.toList());
        } else {
            throw new IllegalStateException("Only type TLS and EthRPC are supported currently.");
        }

        List<CompletableFuture<Identity>> futList = new ArrayList<>();
        int dirIndex = 0;
        for (Map.Entry<Integer, String> subjVar : certSubjectVars) {
            String path = directoryList.get(dirIndex);
            futList.add(CompletableFuture.supplyAsync(() -> SingleBouncyCertificateGenerator
                    .generateIdentity(path, subjVar.getValue(), Integer.toString(subjVar.getKey()))));
            dirIndex++;
        }
        return getWorkResult(futList);
    }

    @Override
    public IdentityFactors getIdentityFactor() {
        return IdentityFactors.newBuilder()
                .setAlgorithm("ECDSA")
                .setCurve("secp384r1")
                .setSigningAlgorithm("SHA384WITHECDSA")
                .build();
    }

    /**
     * Get result from list of completableFutures.
     * @param futures : List of CompletableFuture of {@link Identity}
     * @return : List of {@link Identity}
     */
    private List<Identity> getWorkResult(List<CompletableFuture<Identity>> futures) {
        CompletableFuture<List<Identity>> work = CompletableFuture
                .allOf(futures.toArray(new CompletableFuture[futures.size()]))
                .thenApply(res -> futures.stream().map(CompletableFuture::join).collect(Collectors.toList()));

        List<Identity> result = work.join();

        return result;
    }

    /**
     * create certs in required folder.
     * @return list of the paths created
     */
    private List<String> getCertDirectories(int numCerts, String rootPath) {

        List<String> createDir = IntStream.range(0, numCerts).boxed()
                .map(entry -> String.join("/", rootPath, String.valueOf(entry)))
                .collect(Collectors.toList());

        if (rootPath.equalsIgnoreCase(CONCORD_ETHRPC_SECURITY_IDENTITY_PATH)) {
            return createDir;
        }

        List<String> createSubDir = createDir
                .stream().map(obj -> obj + "/server")
                .collect(Collectors.toList());
        createSubDir.addAll(createDir
                .stream().map(obj -> obj + "/client")
                .collect(Collectors.toList()));

        return createSubDir;
    }


}

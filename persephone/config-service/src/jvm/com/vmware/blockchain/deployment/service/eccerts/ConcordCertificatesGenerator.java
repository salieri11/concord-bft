/*
 * Copyright (c) 2019 VMware, Inc. All rights reserved. VMware Confidential
 */

package com.vmware.blockchain.deployment.service.eccerts;

import java.util.ArrayList;
import java.util.List;
import java.util.concurrent.CompletableFuture;
import java.util.stream.Collectors;
import java.util.stream.IntStream;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.vmware.blockchain.deployment.model.Identity;
import com.vmware.blockchain.deployment.service.generatecerts.CertificatesGenerator;

/**
 * This class is a bouncycastle implementation of getting ssl certs and keypair.
 */
public class ConcordCertificatesGenerator implements CertificatesGenerator {

    private static Logger log = LoggerFactory.getLogger(ConcordCertificatesGenerator.class);

    @Override
    public List<Identity> generateTlsSelfSignedCertificates(int numCerts, String rootPath) {

        List<String> directoryList = getCertDirectories(numCerts, rootPath);

        List<String> cnList = IntStream.range(0, numCerts).boxed()
                .map(entry -> "node" + entry + "ser").collect(Collectors.toList());
        cnList.addAll(IntStream.range(0, numCerts).boxed()
                .map(entry -> "node" + entry + "cli").collect(Collectors.toList()));

        List<CompletableFuture<Identity>> futList = new ArrayList<>();
        int dirIndex = 0;
        for (String cn : cnList) {
            String path = directoryList.get(dirIndex);
            futList.add(CompletableFuture.supplyAsync(() -> SingleBouncyCertificateGenerator
                    .generateIdentity(cn, path, Identity.Type.TLS)));
            dirIndex++;
        }

        return getWorkResult(futList);
    }

    @Override
    public List<Identity> generateEthRpcSelfSignedCertificates(List<String> paths) {

        List<String> cnList = IntStream.range(0, paths.size()).boxed()
                .map(entry -> "node" + entry).collect(Collectors.toList());

        List<CompletableFuture<Identity>> futList = new ArrayList<>();
        int dirIndex = 0;
        for (String cn : cnList) {
            String path = paths.get(dirIndex);
            futList.add(CompletableFuture.supplyAsync(() -> SingleBouncyCertificateGenerator
                    .generateIdentity(cn, path, Identity.Type.ETHRPC)));
            dirIndex++;
        }

        return getWorkResult(futList);
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

        List<String> createSubDir = createDir
                .stream().map(obj -> obj + "/server")
                .collect(Collectors.toList());
        createSubDir.addAll(createDir
                .stream().map(obj -> obj + "/client")
                .collect(Collectors.toList()));

        return createSubDir;
    }


}

/*
 * Copyright (c) 2019 VMware, Inc. All rights reserved. VMware Confidential
 */

package com.vmware.blockchain.deployment.service.server;

import java.io.ByteArrayInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.security.cert.CertificateException;
import java.security.cert.CertificateFactory;
import java.security.cert.X509Certificate;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.List;
import java.util.Map;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.ExecutionException;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.TimeoutException;
import java.util.concurrent.atomic.AtomicInteger;
import java.util.stream.Collectors;

import org.assertj.core.api.Assertions;
import org.junit.jupiter.api.AfterAll;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.Test;

import com.vmware.blockchain.deployment.model.EthRpcConfigurationServiceRequest;
import com.vmware.blockchain.deployment.model.EthRpcConfigurationServiceResponse;
import com.vmware.blockchain.deployment.model.Identity;
import com.vmware.blockchain.deployment.model.TlsConfigurationServiceRequest;
import com.vmware.blockchain.deployment.model.TlsConfigurationServiceResponse;
import com.vmware.blockchain.deployment.service.util.TestUtil;

import io.grpc.stub.StreamObserver;

/**
 * Test for {@link ConfigurationService}.
 */
public class ConfigurationServiceTest {

    /** Default await-time value in milliseconds. */
    private static long awaitTime = 10000;

    private static String filePath = "/tmp/tlsCerts";

    private static ConfigurationService service = newConfigurationService();

    /**
     * Create a new {@link ConfigurationService}.
     *
     * @return
     *   a newly created {@link ConfigurationService} instance.
     */
    private static ConfigurationService newConfigurationService() {
        return DaggerConfigurationServer.create().configurationService();
    }

    /**
     * Create a new {@link StreamObserver} that observes the stream of responses and saves the last
     * response as the result.
     *
     * @param result
     *   the promise to set with result value if result completion or failure were observed.
     * @param <T>
     *   type of result to be observed.
     *
     * @return
     *   a {@link CompletableFuture} that completes upon {@link StreamObserver#onCompleted()} with
     *   the last observed {@link StreamObserver#onNext} signal or completes exceptionally with
     *   {@link StreamObserver#onError} signal.
     */
    private static <T> StreamObserver<T> newResultObserver(CompletableFuture<T> result) {
        return new StreamObserver<>() {
            /** Holder of result value. */
            volatile T value;

            @Override
            public void onNext(T value) {
                this.value = value;
            }

            @Override
            public void onError(Throwable error) {
                result.completeExceptionally(error);
            }

            @Override
            public void onCompleted() {
                result.complete(value);
            }
        };
    }

    /**
     * Create a new {@link StreamObserver} that observes the stream of responses and saves all
     * response into a collection.
     *
     * @param result
     *   the promise to set with result values if result completion or failure were observed.
     * @param <T>
     *   type of result to be observed.
     *
     * @return
     *   a {@link CompletableFuture} that completes upon {@link StreamObserver#onCompleted()} with
     *   the all observed {@link StreamObserver#onNext} signal or completes exceptionally with
     *   {@link StreamObserver#onError} signal.
     */
    private static <T> StreamObserver<T> newCollectingObserver(
            CompletableFuture<Collection<T>> result
    ) {
        return new StreamObserver<>() {
            /**
             * Holder of result values.
             *
             * <p>Note: A map is used here to to leverage existing SDK concurrent data structures
             * without writing a new one. ConcurrentSkipList does not exist in the JDK.
             */
            Map<Integer, T> values = new ConcurrentHashMap<>();

            /** Integer counter. */
            AtomicInteger counter = new AtomicInteger(0);

            @Override
            public void onNext(T value) {
                values.put(counter.getAndIncrement(), value);
            }

            @Override
            public void onError(Throwable error) {
                result.completeExceptionally(error);
            }

            @Override
            public void onCompleted() {
                result.complete(values.values());
            }
        };
    }

    @BeforeAll
    static void setup() throws InterruptedException, ExecutionException, TimeoutException {
        var initialized = service.initialize();
        initialized.get(awaitTime, TimeUnit.MILLISECONDS);
        Assertions.assertThat(initialized).isCompleted();
    }

    @Test
    void testgenerateTlsConfiguration() throws InterruptedException, ExecutionException,
            TimeoutException, IOException, CertificateException {
        List<String> hostIps = new ArrayList<String>();
        hostIps.add("10.0.0.1");
        hostIps.add("10.0.0.2");
        hostIps.add("10.0.0.3");
        hostIps.add("10.0.0.4");

        TlsConfigurationServiceRequest request = new TlsConfigurationServiceRequest(filePath, hostIps);

        var promise = new CompletableFuture<TlsConfigurationServiceResponse>();
        service.generateTlsConfiguration(request, newResultObserver(promise));

        var response = promise.get(awaitTime, TimeUnit.MILLISECONDS);

        assert response.getIdentity().size() == 8;

        var certList = response.getIdentity();
        List<Identity> serverList = certList.stream().limit(4).collect(Collectors.toList());
        List<Identity> clientList = certList.subList(certList.size() - 4, certList.size());
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

        ClassLoader classLoader = getClass().getClassLoader();
        InputStream expectedStream = classLoader.getResourceAsStream("SampleFourNodeConcordConfig.yaml");
        Assertions.assertThat(response.getConfig()).isEqualTo(TestUtil.readFromInputStream(expectedStream));
    }

    @Test
    void testgenerateEthRpcConfiguration() throws ExecutionException, InterruptedException,
            CertificateException, TimeoutException {
        List<String> paths = Arrays.asList(filePath + "/node0", filePath + "/node1", filePath + "/node2");
        EthRpcConfigurationServiceRequest request = new EthRpcConfigurationServiceRequest(paths);

        var promise = new CompletableFuture<EthRpcConfigurationServiceResponse>();
        service.generateEthRpcConfiguration(request, newResultObserver(promise));

        var response = promise.get(awaitTime, TimeUnit.MILLISECONDS);

        assert response.getIdentity().size() == 3;

        var certList = response.getIdentity();
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
    }

    @AfterAll
    static void cleanup() throws InterruptedException, ExecutionException, TimeoutException {
        var shutdown = service.shutdown();
        shutdown.get(awaitTime, TimeUnit.MILLISECONDS);
        Assertions.assertThat(shutdown).isCompleted();
    }
}

/*
 * Copyright (c) 2019 VMware, Inc. All rights reserved. VMware Confidential
 */

package com.vmware.blockchain.deployment.service.server;

import java.security.Security;
import java.util.Objects;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.atomic.AtomicReferenceFieldUpdater;

import org.bouncycastle.jce.provider.BouncyCastleProvider;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.vmware.blockchain.deployment.model.ConfigurationServiceImplBase;
import com.vmware.blockchain.deployment.model.EthRpcConfigurationServiceRequest;
import com.vmware.blockchain.deployment.model.EthRpcConfigurationServiceResponse;
import com.vmware.blockchain.deployment.model.TlsConfigurationServiceRequest;
import com.vmware.blockchain.deployment.model.TlsConfigurationServiceResponse;
import com.vmware.blockchain.deployment.service.eccerts.ConcordCertificatesGenerator;
import com.vmware.blockchain.deployment.service.generatecerts.CertificatesGenerator;
import com.vmware.blockchain.deployment.service.generateconfig.ConcordConfigUtil;

import io.grpc.stub.StreamObserver;

/**
 * Implementation of ConfigurationService server.
 */
public class ConfigurationService extends ConfigurationServiceImplBase {

    /**
     * Enumeration of possible service instance state.
     */
    private enum State {
        STOPPED,
        INITIALIZING,
        ACTIVE,
        STOPPING
    }

    /** Logger instance. */
    private static Logger log = LoggerFactory.getLogger(ConfigurationService.class);

    /** Atomic update for service instance state. */
    private static final AtomicReferenceFieldUpdater<ConfigurationService, State> STATE =
            AtomicReferenceFieldUpdater.newUpdater(ConfigurationService.class, State.class, "state");

    /** Executor to use for all async service operations. */
    private final ExecutorService executor;

    /** Service state. */
    private volatile State state = State.STOPPED;

    /**
     * Constructor.
     */
    ConfigurationService(ExecutorService executor) {
        this.executor = executor;
    }

    /**
     * Initialize the service instance asynchronously.
     *
     * @return
     *   {@link CompletableFuture} that completes when initialization is done.
     */
    CompletableFuture<Void> initialize() {
        if (STATE.compareAndSet(this, State.STOPPED, State.INITIALIZING)) {
            return CompletableFuture.runAsync(() -> {
                Security.addProvider(new BouncyCastleProvider());

                // Set instance to ACTIVE state.
                STATE.set(this, State.ACTIVE);

                log.info("ConfigurationService instance initialized");
            }, executor);
        } else {
            return CompletableFuture.failedFuture(
                    new IllegalStateException("ConfigurationService instance is not in stopped state")
            );
        }
    }

    /**
     * Shutdown the service instance asynchronously.
     *
     * @return
     *   {@link CompletableFuture} that completes when shutdown is done.
     */
    CompletableFuture<Void> shutdown() {
        if (STATE.compareAndSet(this, State.ACTIVE, State.STOPPING)
                || STATE.compareAndSet(this, State.INITIALIZING, State.STOPPING)) {
            return CompletableFuture.runAsync(() -> {
                log.info("ConfigurationService instance shutting down");
                Security.removeProvider("BC");

                // Set instance to STOPPED state.
                STATE.set(this, State.STOPPED);
            }, executor);
        } else {
            return CompletableFuture.failedFuture(
                    new IllegalStateException("ConfigurationService instance is not in active state")
            );
        }
    }

    @Override
    public void generateTlsConfiguration(TlsConfigurationServiceRequest message,
                                         StreamObserver<TlsConfigurationServiceResponse> observer) {

        var request = Objects.requireNonNull(message);
        var response = Objects.requireNonNull(observer);

        CertificatesGenerator certGen = new ConcordCertificatesGenerator();
        TlsConfigurationServiceResponse result = new TlsConfigurationServiceResponse(
                ConcordConfigUtil.generateConfigUtil(request.getHostips()),
                certGen.generateTlsSelfSignedCertificates(request.getHostips().size(), request.getCertPath()));

        response.onNext(result);
        response.onCompleted();
    }

    @Override
    public void generateEthRpcConfiguration(EthRpcConfigurationServiceRequest message,
                                            StreamObserver<EthRpcConfigurationServiceResponse> observer) {

        var request = Objects.requireNonNull(message);
        var response = Objects.requireNonNull(observer);

        CertificatesGenerator certGen = new ConcordCertificatesGenerator();
        EthRpcConfigurationServiceResponse result = new EthRpcConfigurationServiceResponse(
                certGen.generateEthRpcSelfSignedCertificates(request.getCertPath()));

        response.onNext(result);
        response.onCompleted();
    }
}

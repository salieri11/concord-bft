/*
 * Copyright (c) 2019 VMware, Inc. All rights reserved. VMware Confidential
 */

package com.vmware.blockchain.deployment.provision.client;

import java.util.Collection;
import java.util.List;
import java.util.Map;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.atomic.AtomicInteger;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.vmware.blockchain.deployment.model.ConcordComponent;
import com.vmware.blockchain.deployment.model.ConcordModelSpecification;
import com.vmware.blockchain.deployment.model.CreateClusterRequest;
import com.vmware.blockchain.deployment.model.DeploymentSessionIdentifier;
import com.vmware.blockchain.deployment.model.DeploymentSpecification;
import com.vmware.blockchain.deployment.model.MessageHeader;
import com.vmware.blockchain.deployment.model.OrchestrationSiteIdentifier;
import com.vmware.blockchain.deployment.model.PlacementSpecification;
import com.vmware.blockchain.deployment.model.PlacementSpecification.Entry;
import com.vmware.blockchain.deployment.model.ProvisionServiceStub;
import com.vmware.blockchain.deployment.model.ethereum.Genesis;

import io.grpc.CallOptions;
import io.grpc.ManagedChannel;
import io.grpc.ManagedChannelBuilder;
import io.grpc.stub.StreamObserver;

/**
 * Simple gRPC client for accessing Provisioning-Service.
 */
public class ProvisionServiceClient {

    private static Logger log = LoggerFactory.getLogger ( ProvisionServiceClient.class );
    private static String USAGE = "provision-client createCluster <server:port> <JSON>";
    private static String CREATE_CLUSTER = "createCluster";
    private static long awaitTime = 10000;


    /**
     * Taken from "Test" code.
     */
    private static <T> StreamObserver<T> newResultObserver(CompletableFuture<T> result) {
        return new StreamObserver<>() {
            /** Holder of result value. */
            volatile T value = null;

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
     * Another listener for collection kind of response.
     */
    private static <T> StreamObserver<T> newCollectingObserver(CompletableFuture<Collection<T>> result) {
        return new StreamObserver<>() {
            /**
             * Holder of result values.
             *
             * Note: A map is used here to to leverage existing SDK concurrent data structures
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

    /**
     * The actual call which will contact server and add the model request.
     */
    private static boolean createFixedSizeCluster(String address) {
        try {
            // Create a channel
            ManagedChannel channel = ManagedChannelBuilder.forTarget(address).usePlaintext().build ();


            // Create a blocking stub with the channel
            var client = new ProvisionServiceStub (channel, CallOptions.DEFAULT);
            var cluster_size = 7;
            var list = List.of(
                    new Entry(PlacementSpecification.Type.FIXED, new OrchestrationSiteIdentifier(1,0)),
                    new Entry(PlacementSpecification.Type.FIXED, new OrchestrationSiteIdentifier(2,0)),
                    new Entry(PlacementSpecification.Type.FIXED, new OrchestrationSiteIdentifier(1,0)),
                    new Entry(PlacementSpecification.Type.FIXED, new OrchestrationSiteIdentifier(2,0)),
                    new Entry(PlacementSpecification.Type.FIXED, new OrchestrationSiteIdentifier(1,0)),
                    new Entry(PlacementSpecification.Type.FIXED, new OrchestrationSiteIdentifier(2,0)),
                    new Entry(PlacementSpecification.Type.FIXED, new OrchestrationSiteIdentifier(1,0))
             );
            /*
            var list = List.of(
                    new Entry(PlacementSpecification.Type.UNSPECIFIED, new OrchestrationSiteIdentifier(1,2)),
                    new Entry(PlacementSpecification.Type.UNSPECIFIED, new OrchestrationSiteIdentifier(2,3)),
                    new Entry(PlacementSpecification.Type.UNSPECIFIED, new OrchestrationSiteIdentifier(3,4)),
                    new Entry(PlacementSpecification.Type.UNSPECIFIED, new OrchestrationSiteIdentifier(4,5))
            );
            */
            var placementSpec = new PlacementSpecification(list);
            var components = List.of(
                    new ConcordComponent(ConcordComponent.Type.DOCKER_IMAGE, "vmwblockchain/concord-core:latest"),
                    new ConcordComponent(ConcordComponent.Type.DOCKER_IMAGE, "vmwblockchain/ethrpc:latest"),
                    new ConcordComponent(ConcordComponent.Type.DOCKER_IMAGE, "vmwblockchain/agent-testing:latest")
            );
            var genesis = new Genesis(
                    new Genesis.Config(1, 0, 0, 0),
                    "0x0000000000000000",
                    "0x400",
                    "0x0000000000000000000000000000000000000000000000000000000000000000",
                    "0x0000000000000000000000000000000000000000000000000000000000000000",
                    "0xf4240",
                    Map.of(
                            "262c0d7ab5ffd4ede2199f6ea793f819e1abb019", new Genesis.Wallet("12345"),
                            "5bb088f57365907b1840e45984cae028a82af934", new Genesis.Wallet("0xabcdef"),
                            "0000a12b3f3d6c9b0d3f126a83ec2dd3dad15f39", new Genesis.Wallet("0x7fffffffffffffff")
                    )
            );
            ConcordModelSpecification spec = new ConcordModelSpecification(
                    "photon-3.0-64",
                    // "5b7eac22-976e-47fa-a000-1e09020a1c5d",
                    "8abc7fda-9576-4b13-9beb-06f867cf2c7c",
                    components
            );
            DeploymentSpecification deploySpec =
                    new DeploymentSpecification(cluster_size, spec, placementSpec, genesis);
            var request = new CreateClusterRequest (new MessageHeader(), deploySpec );
            // Check that the API can be serviced normally after service initialization.
            var promise = new CompletableFuture<DeploymentSessionIdentifier> ();
            client.createCluster (request , newResultObserver(promise));
            var response = promise.get (awaitTime , TimeUnit.MILLISECONDS );
            log.info("response.getLow:" + response.getLow());
            log.info("response.getHigh:" + response.getHigh());
            return true;
        } catch (Throwable error) {
            log.error("Couldn't create client properly", error);
            return false;
        }
    }

    public static void main(String[] argv) {
        createFixedSizeCluster(argv[0]);
    }
}

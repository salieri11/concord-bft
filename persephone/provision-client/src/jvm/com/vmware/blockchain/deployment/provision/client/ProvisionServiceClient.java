/*
 * Copyright (c) 2019 VMware, Inc. All rights reserved. VMware Confidential
 */

package com.vmware.blockchain.deployment.client.provision;

import java.util.Collection;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.ExecutionException;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.TimeoutException;
import java.util.concurrent.atomic.AtomicInteger;
import java.util.stream.Collectors;

import com.vmware.blockchain.deployment.model.ConcordCluster;
import com.vmware.blockchain.deployment.model.ConcordClusterIdentifier;
import com.vmware.blockchain.deployment.model.ConcordComponent;
import com.vmware.blockchain.deployment.model.ConcordModelSpecification;
import com.vmware.blockchain.deployment.model.ConcordNode;
import com.vmware.blockchain.deployment.model.ConcordNodeHostInfo;
import com.vmware.blockchain.deployment.model.ConcordNodeIdentifier;
import com.vmware.blockchain.deployment.model.ConcordNodeInfo;
import com.vmware.blockchain.deployment.model.ConcordNodeStatus;
import com.vmware.blockchain.deployment.model.CreateClusterRequest;
import com.vmware.blockchain.deployment.model.DeploymentSession;
import com.vmware.blockchain.deployment.model.DeploymentSessionEvent;
import com.vmware.blockchain.deployment.model.DeploymentSessionIdentifier;
import com.vmware.blockchain.deployment.model.DeploymentSpecification;
import com.vmware.blockchain.deployment.model.MessageHeader;
import com.vmware.blockchain.deployment.model.OrchestrationSiteIdentifier;
import com.vmware.blockchain.deployment.model.PlacementSpecification;
import com.vmware.blockchain.deployment.model.PlacementSpecification.PlacementEntry;
import com.vmware.blockchain.deployment.model.ProvisionServiceStub;


import io.grpc.CallOptions;
import io.grpc.ManagedChannel;
import io.grpc.ManagedChannelBuilder;
import io.grpc.stub.StreamObserver;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

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
            var cluster_size = 4;
            List<PlacementEntry> list = new ArrayList<PlacementEntry>(4);
            list.add(new PlacementEntry(PlacementSpecification.Type.UNSPECIFIED, new OrchestrationSiteIdentifier(1,2)));
            list.add(new PlacementEntry(PlacementSpecification.Type.UNSPECIFIED, new OrchestrationSiteIdentifier(2,3)));
            list.add(new PlacementEntry(PlacementSpecification.Type.UNSPECIFIED, new OrchestrationSiteIdentifier(3,4)));
            list.add(new PlacementEntry(PlacementSpecification.Type.UNSPECIFIED, new OrchestrationSiteIdentifier(4,5)));
            var placementSpec = new PlacementSpecification(list);
            List<ConcordComponent> components = new ArrayList<ConcordComponent>();
            ConcordModelSpecification spec = new ConcordModelSpecification("version1", "template", components);
            DeploymentSpecification deploySpec = new DeploymentSpecification(cluster_size, spec, placementSpec);
            var request = new CreateClusterRequest (new MessageHeader(), deploySpec );
            // Check that the API can be serviced normally after service initialization.
            var promise = new CompletableFuture<DeploymentSessionIdentifier> ();
            client.createCluster (request , newResultObserver(promise));
            var response = promise.get (awaitTime , TimeUnit.MILLISECONDS );
            log.info("response.getLow:" + response.getLow());
            log.info("response.getHigh:" + response.getHigh());
            return true;
        } catch (InterruptedException e) {
            e.printStackTrace ();
            log.error("Couldn't create client properly1");
            return false;
        } catch (ExecutionException e) {
            e.printStackTrace ();
            log.error("Execution exception");
            return false;
        } catch (TimeoutException e) {
            e.printStackTrace ();
            log.error("Couldn't create client properly3");
            return false;
        }
    }

    public static void main(String[] argv) {
        createFixedSizeCluster(argv[0]);
    }
}

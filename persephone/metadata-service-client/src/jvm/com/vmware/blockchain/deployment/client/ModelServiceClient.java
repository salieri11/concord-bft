/*
 * Copyright (c) 2019 VMware, Inc. All rights reserved. VMware Confidential
 */

package com.vmware.blockchain.deployment.client;

import java.util.ArrayList;
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

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.vmware.blockchain.deployment.model.AddModelRequest;
import com.vmware.blockchain.deployment.model.AddModelResponse;
import com.vmware.blockchain.deployment.model.ConcordComponent;
import com.vmware.blockchain.deployment.model.ConcordModelServiceStub;
import com.vmware.blockchain.deployment.model.ConcordModelSpecification;
import com.vmware.blockchain.deployment.model.ListModelsRequest;
import com.vmware.blockchain.deployment.model.ListModelsResponseEvent;
import com.vmware.blockchain.deployment.model.MessageHeader;

import io.grpc.CallOptions;
import io.grpc.ManagedChannel;
import io.grpc.ManagedChannelBuilder;
import io.grpc.stub.StreamObserver;

/**
 * Simple gRPC client for accessing Model-Service.
 */
public class ModelServiceClient {

    private static Logger log = LoggerFactory.getLogger(ModelServiceClient.class);
    private static String USAGE = "model-client addModel <server:port> <versioNo> <templateNo> "
                                + "<imagename,imagename,...> OR model-client listModel <server:port>";
    private static String ADD_MODEL = "addModel";
    private static String LiST_MODEL = "listModel";
    private static int MAX_RETURN = 10;
    private static boolean isAddModel;
    private static long awaitTime = 10000;


    /**
     * Taken from "Test" code.
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
     * Another listener for collection kind of response.
     */
    private static <T> StreamObserver<T> newCollectingObserver(CompletableFuture<Collection<T>> result) {
        return new StreamObserver<>() {
            /**
             * Holder of result values.
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
     * Simple parse user-input.
     */
    private static boolean parseInput(String[] argv) {
        if (argv == null || argv.length == 0) {
            return false;
        }
        if (argv[0].equals(ADD_MODEL)) {
            if (argv.length != 5) {
                return false;
            }
            isAddModel = true;
            return true;
        } else if (argv[0].equals(LiST_MODEL)) {
            if (argv.length != 2) {
                return false;
            }
            isAddModel = false;
            return true;
        }
        return false;
    }


    /**
     * The actual call which will contact server and add the model request.
     */
    private static boolean addModel(String address, String version, String templateVersion, String images) {
        try {
            // Create a channel
            ManagedChannel channel = ManagedChannelBuilder.forTarget(address).usePlaintext().build();

            // Create a blocking stub with the channel
            var client = new ConcordModelServiceStub(channel, CallOptions.DEFAULT);

            // Check that the API can be serviced normally after service initialization.
            var promise = new CompletableFuture<AddModelResponse>();
            List<ConcordComponent> concordComponents = new ArrayList<ConcordComponent>();
            var imageSplit = images.split(", ");
            for (String image : imageSplit) {
                ConcordComponent c = new ConcordComponent(ConcordComponent.Type.DOCKER_IMAGE, image);
                concordComponents.add(c);
            }
            var model = new ConcordModelSpecification(version, templateVersion, concordComponents);
            var request = new AddModelRequest(new MessageHeader(), model);

            client.addModel(request, newResultObserver(promise));
            var response = promise.get(awaitTime, TimeUnit.MILLISECONDS);
            System.out.println("response.getId:" + response.getId());
            System.out.println("response:" + response.toString());

        } catch (InterruptedException e) {
            e.printStackTrace();
            System.out.println("Couldn't create client properly");
            log.error("Couldn't create client properly");
            return false;
        } catch (ExecutionException e) {
            e.printStackTrace();
            System.out.println("Couldn't create client properly");
            log.error("Couldn't create client properly");
            return false;
        } catch (TimeoutException e) {
            e.printStackTrace();
            System.out.println("Couldn't create client properly");
            log.error("Couldn't create client properly");
            return false;
        }
        return true;
    }

    private static boolean listModel(String address) {
        try {
            // Create a channel
            ManagedChannel channel = ManagedChannelBuilder.forTarget(address).usePlaintext().build();

            // Create a blocking stub with the channel
            var client = new ConcordModelServiceStub(channel, CallOptions.DEFAULT);

            // Obtain the list of added models.
            var listRequest = new ListModelsRequest(new MessageHeader(),
                                                    ListModelsRequest.OrderBy.UNSPECIFIED,
                                                    MAX_RETURN);

            var listPromise = new CompletableFuture<Collection<ListModelsResponseEvent>>();
            client.listModels(listRequest, newCollectingObserver(listPromise));
            var result = listPromise.get(awaitTime, TimeUnit.MILLISECONDS).stream()
                                    .collect(Collectors.toMap(ListModelsResponseEvent::getId,
                                                              ListModelsResponseEvent::getSpecification));

            System.out.println("response:" + result.toString());

        } catch (InterruptedException e) {
            e.printStackTrace();
            System.out.println("Couldn't create client properly");
            log.error("Couldn't create client properly");
            return false;
        } catch (ExecutionException e) {
            e.printStackTrace();
            System.out.println("Couldn't create client properly");
            log.error("Couldn't create client properly");
            return false;
        } catch (TimeoutException e) {
            e.printStackTrace();
            System.out.println("Couldn't create client properly");
            log.error("Couldn't create client properly");
            return false;
        }
        return true;
    }

    /**
     * Main - CLI utility function.
     */
    public static void main(String[] argv) {
        if (!parseInput(argv)) {
            System.out.println("Invalid arguments. Correct usage: " + USAGE);
            log.error("Invalid arguments. Correct usage: " + USAGE);
            return;
        }
        if (isAddModel) {
            addModel(argv[1], argv[2], argv[3], argv[4]);
        } else {
            listModel(argv[1]);
        }
    }
}

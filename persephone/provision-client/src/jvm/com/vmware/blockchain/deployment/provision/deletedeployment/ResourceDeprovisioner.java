/*
 * Copyright (c) 2019 VMware, Inc. All rights reserved. VMware Confidential
 */

package com.vmware.blockchain.deployment.provision.deletedeployment;

import java.net.URI;
import java.net.URL;
import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Map;

import java.util.concurrent.CompletableFuture;
import java.util.concurrent.ExecutionException;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.ForkJoinPool;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.TimeoutException;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.vmware.blockchain.deployment.model.DeploymentSessionEvent;
import com.vmware.blockchain.deployment.model.DeploymentSessionIdentifier;
import com.vmware.blockchain.deployment.model.MessageHeader;
import com.vmware.blockchain.deployment.model.OrchestrationSiteIdentifier;
import com.vmware.blockchain.deployment.model.OrchestrationSiteInfo;
import com.vmware.blockchain.deployment.model.ProvisionedResource;
import com.vmware.blockchain.deployment.model.ProvisioningServiceStub;
import com.vmware.blockchain.deployment.model.StreamClusterDeploymentSessionEventRequest;
import com.vmware.blockchain.deployment.orchestration.Orchestrator;
import com.vmware.blockchain.deployment.provision.common.Observers;
import com.vmware.blockchain.deployment.provision.common.OrchestrationProviders;

import io.grpc.CallOptions;
import io.grpc.ManagedChannel;
import io.grpc.ManagedChannelBuilder;
import kotlin.Pair;
import kotlinx.serialization.UpdateMode;
import kotlinx.serialization.json.Json;
import kotlinx.serialization.json.JsonConfiguration;
import kotlinx.serialization.modules.EmptyModule;

/**
 * Class to hold delete resource cli elements.
 */
public class ResourceDeprovisioner {
    private static Logger log = LoggerFactory.getLogger(ResourceDeprovisioner.class);
    private final Map<OrchestrationSiteIdentifier, Integer> mapOrchestrator = new HashMap<>();
    private final List<CompletableFuture<Orchestrator>> orchestratorFutureList = new ArrayList<>();
    private final ExecutorService executor = ForkJoinPool.commonPool();
    private final OrchestrationProviders orchestrationProvider = new OrchestrationProviders(executor);
    private DeploymentSessionIdentifier id;
    private String address;
    private static long awaitTime = 100000;

    /**
     * ResourceDeprovisioner constructor.
     * @param id deploymentSessionIdentifier
     * @param config String config with the credentials
     * @param address ip and port of type ip:port
     */
    public ResourceDeprovisioner(DeploymentSessionIdentifier id, String config, String address) {
        this.id = id;
        this.address = address;
        this.initializeOrchestrations(config);
    }

    private void initializeOrchestrations(String config) {

        var json = new Json(
                new JsonConfiguration(
                        false, /* encodeDefaults */
                        true, /* strictMode */
                        false, /* unquoted */
                        false, /* prettyPrint */
                        "    ", /* indent */
                        false, /* useArrayPolymorphism */
                        "type", /* classDiscriminator */
                        UpdateMode.OVERWRITE /* updateMode */
                ),
                EmptyModule.INSTANCE
        );

        /* Below could be temporary until we implement a serializer that takes in
        current config structure to give a orchestration site id to site info map
         */
        var siteObj = json.parseJson(config).getJsonObject().get("sites");
        siteObj.getJsonArray().stream().forEach((entry) -> {
            var orchestrationSiteId = json.parse(OrchestrationSiteIdentifier.getSerializer(),
                    entry.getJsonObject().get("id").toString());
            var orchestrationSiteInfo = json.parse(OrchestrationSiteInfo.getSerializer(),
                    entry.getJsonObject().get("info").toString());
            orchestratorFutureList
                    .add(orchestrationProvider.newOrchestrator(orchestrationSiteInfo));
            mapOrchestrator.put(orchestrationSiteId, (orchestratorFutureList.size() - 1));
        });
    }

    private boolean isValid(String url) {
        try {
            new URL(url).toURI();
            return true;
        } catch (Exception e) {
            return false;
        }
    }

    /**
     * deletes the cluster resources.
     */
    public void delete() {
        ManagedChannel channel = ManagedChannelBuilder.forTarget(address).usePlaintext().build();
        var client = new ProvisioningServiceStub(channel, CallOptions.DEFAULT);

        var promise2 = new CompletableFuture<Collection<DeploymentSessionEvent>>();

        var sessionEvents = new StreamClusterDeploymentSessionEventRequest(
                new MessageHeader(),
                id
        );

        log.info("sessionEvents " + sessionEvents);
        client.streamClusterDeploymentSessionEvents(
                sessionEvents,
                Observers.newCollectingObserver(promise2)
        );

        // Assuming deletion is triggered after all events are populated already for that session
        Collection<DeploymentSessionEvent> events = null;
        try {
            events = promise2.get(awaitTime, TimeUnit.MILLISECONDS);
        } catch (InterruptedException | ExecutionException | TimeoutException e) {
            log.error("Delete resource failed.");
            e.printStackTrace();
            throw new RuntimeException(e);
        }

        log.info("Event: {}".format(String.valueOf(events)));

        List<Pair<Integer, URI>> networkAllocList = new ArrayList<>();
        List<Pair<Integer, URI>> computeList = new ArrayList<>();
        List<Pair<Integer, URI>> networkAddrList = new ArrayList<>();

        Iterator it = events.iterator();
        while (it.hasNext()) {
            DeploymentSessionEvent val = (DeploymentSessionEvent) it.next();

            if (!val.getType().equals(DeploymentSessionEvent.Type.RESOURCE)) {
                continue;
            }
            String name = val.getResource().getName();
            if (isValid(name)) {
                URI resourceUri = URI.create(name);
                OrchestrationSiteIdentifier site = val.getResource().getSite();
                ProvisionedResource.Type resourceType = val.getResource().getType(); //netalloc
                if (resourceType.equals(ProvisionedResource.Type.NETWORK_ALLOCATION)) {
                    networkAllocList.add(new Pair<>(mapOrchestrator.get(site), resourceUri));
                }
                if (resourceType.equals(ProvisionedResource.Type.COMPUTE_RESOURCE)) {
                    computeList.add(new Pair<>(mapOrchestrator.get(site), resourceUri));
                }
                if (resourceType.equals(ProvisionedResource.Type.NETWORK_RESOURCE)) {
                    networkAddrList.add(new Pair<>(mapOrchestrator.get(site), resourceUri));
                }
            }

            System.out.println("value: " + val);
        }

        CompletableFuture finalWork =
                CompletableFuture.allOf(orchestratorFutureList.stream().toArray(CompletableFuture[]::new))
                .thenRunAsync(() -> {
                    CompletableFuture.allOf(getWork(networkAllocList,
                            ProvisionedResource.Type.NETWORK_ALLOCATION)
                            .stream().toArray(CompletableFuture[]::new))
                            .whenComplete((result, ex) ->
                                log.info("Network Allocation Deletion completed\n"
                                        + "result: " + result + " " + ex)
                            ).join();
                    CompletableFuture.allOf(getWork(computeList,
                            ProvisionedResource.Type.COMPUTE_RESOURCE)
                            .stream().toArray(CompletableFuture[]::new))
                            .whenComplete((result, ex) ->
                                log.info("Compute Resource Deletion completed\n"
                                        + "result: " + result + " " + ex)
                            ).join();
                    CompletableFuture.allOf(getWork(networkAddrList,
                            ProvisionedResource.Type.NETWORK_RESOURCE)
                            .stream().toArray(CompletableFuture[]::new))
                            .whenComplete((result, ex) ->
                                log.info("Network Address Deletion completed\n"
                                        + "result: " + result + " " + ex)
                            ).join();
                }).whenComplete((result, ex) ->
                    log.info("Deletion completed\n"
                            + "result: " + result + " " + ex)
                );
        finalWork.join();
    }

    private List<CompletableFuture> getWork(List<Pair<Integer, URI>> workList,
                                            ProvisionedResource.Type type) {
        List<CompletableFuture> workResults = new ArrayList<>();
        for (Pair<Integer, URI> work : workList) {
            Orchestrator orchestrator = orchestratorFutureList.get(work.getFirst()).join();
            workResults.add(
                    new DeleteResource(work.getSecond(), orchestrator, type).delete());
        }
        return workResults;
    }

}

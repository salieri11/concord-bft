/*
 * Copyright (c) 2020 VMware, Inc. All rights reserved. VMware Confidential
 */

package com.vmware.blockchain.deployment.services.orchestration.vsphere;


import java.net.InetAddress;
import java.net.URI;
import java.util.Map;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.Flow;

import com.google.common.net.InetAddresses;
import com.vmware.blockchain.deployment.services.exception.BadRequestPersephoneException;
import com.vmware.blockchain.deployment.services.exception.PersephoneException;
import com.vmware.blockchain.deployment.services.orchestration.Orchestrator;
import com.vmware.blockchain.deployment.services.orchestration.OrchestratorData;
import com.vmware.blockchain.deployment.services.orchestration.ipam.IpamClient;
import com.vmware.blockchain.deployment.services.orchestration.vm.CloudInitConfiguration;
import com.vmware.blockchain.deployment.v1.ConcordClusterIdentifier;
import com.vmware.blockchain.deployment.v1.VSphereOrchestrationSiteInfo;

import lombok.extern.slf4j.Slf4j;
import lombok.val;

/**
 * Deployment orchestration driver for VMware vSphere environment.
 */
@Slf4j
public class VSphereOrchestrator implements Orchestrator {

    private VSphereOrchestrationSiteInfo info;
    private IpamClient ipamClient;
    private VSphereHttpClient vSphereHttpClient;

    /**
     * Constructor.
     */
    public VSphereOrchestrator(VSphereOrchestrationSiteInfo info, VSphereHttpClient vSphereHttpClient,
                               IpamClient ipamClient) {
        this.info = info;
        this.ipamClient = ipamClient;
        this.vSphereHttpClient = vSphereHttpClient;
    }

    @Override
    public boolean validate() {
        // TODO Could be made async.
        try {
            vSphereHttpClient.getDatastore(info.getVsphere().getDatastore());
            vSphereHttpClient.getResourcePool(info.getVsphere().getResourcePool());
            vSphereHttpClient.getFolder(info.getVsphere().getFolder());
        } catch (PersephoneException e) {
            log.info("Error validating the zone info");
            throw new BadRequestPersephoneException("Error validating site: ", e);
        }
        return true;
    }

    @Override
    public OrchestratorData.ComputeResourceEvent createDeployment(
            OrchestratorData.CreateComputeResourceRequest request) {
        val compute = info.getVsphere().getResourcePool();
        val storage = info.getVsphere().getDatastore();
        val network = info.getVsphere().getNetwork();

        try {
            val getFolder = CompletableFuture
                    .supplyAsync(() -> vSphereHttpClient.getFolder(info.getVsphere().getFolder()));
            val getDatastore = CompletableFuture.supplyAsync(() -> vSphereHttpClient.getDatastore(storage));
            val getResourcePool = CompletableFuture.supplyAsync(() -> vSphereHttpClient.getResourcePool(compute));
            val getControlNetwork =
                    CompletableFuture
                            .supplyAsync(() -> vSphereHttpClient.getNetwork(network.getName(), "OPAQUE_NETWORK"));
            val getLibraryItem =
                    CompletableFuture
                            .supplyAsync(() -> vSphereHttpClient.getLibraryItem(request.getModel().getTemplate()));
            // Collect all information and deploy.
            val folder = getFolder.get();
            val datastore = getDatastore.get();
            val resourcePool = getResourcePool.get();
            val controlNetwork = getControlNetwork.get();
            val libraryItem = getLibraryItem.get();
            val cloudInit = new CloudInitConfiguration(
                                    info.getContainerRegistry(),
                                    request.getModel(),
                                    request.getPrivateNetworkAddress(),
                                    InetAddress.getByName(String.valueOf(network.getGateway()))
                                            .getHostAddress(),
                                    network.getNameServersList(),
                                    network.getSubnet(),
                                    request.getCluster(),
                                    request.getConcordId(),
                                    request.getConfigurationSessionIdentifier(),
                                    request.getConfigServiceEndpoint(),
                                    request.getConfigServiceRestEndpoint(),
                                    info.getVsphere().getOutboundProxy()
                                );

            val instance = vSphereHttpClient
                    .createVirtualMachine(request.getCluster().getId() + "-" + request.getNode().getId(),
                                          libraryItem, datastore, resourcePool, folder,
                                          Map.entry("blockchain-network", controlNetwork), cloudInit);

            vSphereHttpClient.ensureVirtualMachinePowerStart(instance, 5000L, request.getProperties());
            return OrchestratorData.ComputeResourceEventCreated.builder()
                    .resource(vSphereHttpClient.vmIdAsUri(instance))
                    .node(request.getNode())
                    .password(cloudInit.getVmPassword())
                    .build();
        } catch (Exception e) {
            throw new PersephoneException(e, "Error creating/starting the VM");
        }
    }

    @Override
    public OrchestratorData.ComputeResourceEventCreatedV2 createDeploymentV2(
            OrchestratorData.CreateComputeResourceRequestV2 request) {
        val compute = info.getVsphere().getResourcePool();
        val storage = info.getVsphere().getDatastore();
        val network = info.getVsphere().getNetwork();

        try {
            val getFolder = CompletableFuture
                    .supplyAsync(() -> vSphereHttpClient.getFolder(info.getVsphere().getFolder()));
            val getDatastore = CompletableFuture.supplyAsync(() -> vSphereHttpClient.getDatastore(storage));
            val getResourcePool = CompletableFuture.supplyAsync(() -> vSphereHttpClient.getResourcePool(compute));
            val getControlNetwork =
                    CompletableFuture
                            .supplyAsync(() -> vSphereHttpClient.getNetwork(network.getName(), "OPAQUE_NETWORK"));
            val getLibraryItem =
                    CompletableFuture
                            .supplyAsync(() -> vSphereHttpClient.getLibraryItem(request.getCloudInitData()
                                                                                        .getModel().getTemplate()));
            // Collect all information and deploy.
            val folder = getFolder.get();
            val datastore = getDatastore.get();
            val resourcePool = getResourcePool.get();
            val controlNetwork = getControlNetwork.get();
            val libraryItem = getLibraryItem.get();
            val cloudInit = new CloudInitConfiguration(
                    info.getContainerRegistry(),
                    request.getCloudInitData().getModel(),
                    request.getCloudInitData().getPrivateIp(),
                    InetAddress.getByName(String.valueOf(network.getGateway()))
                            .getHostAddress(),
                    network.getNameServersList(),
                    network.getSubnet(),
                    ConcordClusterIdentifier.newBuilder()
                            .setId(request.getBlockchainId().toString())
                            .build(),
                    request.getCloudInitData().getNodeId(),
                    request.getCloudInitData().getConfigGenId(),
                    request.getCloudInitData().getConfigServiceEndpoint(),
                    request.getCloudInitData().getConfigServiceRestEndpoint(),
                    info.getVsphere().getOutboundProxy()
            );
            val instance = vSphereHttpClient
                    .createVirtualMachine(request.getVmId(),
                                          libraryItem, datastore, resourcePool, folder,
                                          Map.entry("blockchain-network", controlNetwork),
                                          cloudInit);

            // Temp for now
            vSphereHttpClient.ensureVirtualMachinePowerStart(instance, 5000L, request.getProperties());
            return OrchestratorData.ComputeResourceEventCreatedV2.builder()
                    .resource(vSphereHttpClient.vmIdAsUri(instance))
                    .password(cloudInit.getVmPassword())
                    .nodeId(request.getNodeId()).build();
        } catch (Exception e) {
            throw new PersephoneException(e, "Error creating/starting the VM");
        }
    }

    @Override
    public OrchestratorData.ComputeResourceEvent deleteDeployment(
            OrchestratorData.DeleteComputeResourceRequest request) {

        String resourceLink = request.getResource().getPath();
        // Retrieve only the last portion of the URI to get the VM ID.
        val id = resourceLink.substring(resourceLink.lastIndexOf("/"));

        // Ensure that the VM is powered off.
        if (vSphereHttpClient.ensureVirtualMachinePowerStop(id)) {
            vSphereHttpClient.deleteVirtualMachine(id);

            return new OrchestratorData.ComputeResourceEvent(request.getResource());
        }
        throw new PersephoneException("Unable to power off VM: " + request.getResource());
    }

    @Override
    public Flow.Publisher<OrchestratorData.NetworkResourceEvent> createNetworkAddress(
            OrchestratorData.CreateNetworkResourceRequest request) {
        return publisher -> {

            val privateIpAddress = ipamClient.allocatedPrivateIp(info.getVsphere().getNetwork().getName());

            if (privateIpAddress != null) {
                log.info("Assigned private IP {}", privateIpAddress);
                val event = OrchestratorData.NetworkResourceEventCreated.builder()
                        .name(request.getName())
                        .address(InetAddresses.fromInteger(privateIpAddress.getValue()).getHostAddress())
                        .publicResource(false)
                        .resource(URI.create("/" + privateIpAddress.getName()))
                        .build();

                publisher.onNext(event);
            }

            // If request asked for public IP provisioning as well, send an event.
            if (request.getPublicResource()) {
                val publicAddressEvent = OrchestratorData.NetworkResourceEventCreated.builder()
                        .name(request.getName())
                        .address(InetAddresses.fromInteger(privateIpAddress.getValue()).getHostAddress())
                        .publicResource(true)
                        .resource(URI.create("/" + privateIpAddress.getName()))
                        .build();
                publisher.onNext(publicAddressEvent);
            }
            publisher.onComplete();
        };
    }

    @Override
    public OrchestratorData.NetworkResourceEvent createPrivateNetworkAddress(
            OrchestratorData.CreateNetworkResourceRequest request) {

        val privateIpAddress = ipamClient.allocatedPrivateIp(info.getVsphere().getNetwork().getName());
        log.info("Assigned private IP {}", privateIpAddress);

        return OrchestratorData.NetworkResourceEventCreated.builder()
                .name(request.getName())
                .address(InetAddresses.fromInteger(privateIpAddress.getValue()).getHostAddress())
                .publicResource(false)
                .resource(URI.create("/" + privateIpAddress.getName()))
                .build();
    }

    @Override
    public Flow.Publisher<OrchestratorData.NetworkResourceEvent> deleteNetworkAddress(
            OrchestratorData.DeleteNetworkResourceRequest request) {

        return publisher -> {
            if (ipamClient.releasePrivateIp(request.getResource())) {
                log.info("Released private IP resource {}", request.getResource());

                val event = new OrchestratorData.NetworkResourceEvent(request.getResource());
                publisher.onNext(event);
                publisher.onComplete();
            } else {
                publisher
                        .onError(new PersephoneException("Unable to delete network address {}", request.getResource()));
            }
        };
    }

    @Override
    public Flow.Publisher<OrchestratorData.NetworkAllocationEvent> createNetworkAllocation(
            OrchestratorData.CreateNetworkAllocationRequest request) {
        return subscriber -> {
            val event = OrchestratorData.NetworkAllocationEventCreated.builder()
                    .name(request.getName())
                    .compute(request.getCompute())
                    .publicNetwork(request.getPublicNetwork())
                    .privateNetwork(request.getPrivateNetwork())
                    .build();

            event.setResource(URI.create("///network-allocations/" + request.getName()));
            subscriber.onNext(event);
            subscriber.onComplete();
        };
    }

    @Override
    public Flow.Publisher<OrchestratorData.NetworkAllocationEvent> deleteNetworkAllocation(
            OrchestratorData.DeleteNetworkAllocationRequest request) {

        return publisher -> {
            val event = new OrchestratorData.NetworkAllocationEvent(request.getResource());
            publisher.onNext(event);
            publisher.onComplete();
        };
    }
}

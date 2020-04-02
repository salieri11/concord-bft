/*
 * Copyright (c) 2020 VMware, Inc. All rights reserved. VMware Confidential
 */

package com.vmware.blockchain.deployment.services.orchestration.vsphere;


import java.net.InetAddress;
import java.net.URI;
import java.util.Map;
import java.util.UUID;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.Flow;

import com.vmware.blockchain.deployment.services.exception.BadRequestPersephoneException;
import com.vmware.blockchain.deployment.services.exception.PersephoneException;
import com.vmware.blockchain.deployment.services.orchestration.NetworkAddress;
import com.vmware.blockchain.deployment.services.orchestration.Orchestrator;
import com.vmware.blockchain.deployment.services.orchestration.OrchestratorData;
import com.vmware.blockchain.deployment.services.orchestration.ipam.IpamClient;
import com.vmware.blockchain.deployment.services.orchestration.vm.CloudInitConfiguration;
import com.vmware.blockchain.deployment.v1.OutboundProxyInfo;
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
    public void initialize() {

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
            val clusterId = new UUID(request.getCluster().getHigh(), request.getCluster().getLow());
            val nodeId = new UUID(request.getNode().getHigh(), request.getNode().getLow());

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

            val instance = vSphereHttpClient
                    .createVirtualMachine(clusterId + "-" + nodeId, libraryItem, datastore, resourcePool, folder,
                                          Map.entry("blockchain-network", controlNetwork),
                                          new CloudInitConfiguration(
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
                                                  OutboundProxyInfo.newBuilder().build(),
                                                  info.getWavefront()
                                          ),
                                          request.getVmProfile()
                    );

            vSphereHttpClient.ensureVirtualMachinePowerStart(instance, 5000L);
            return OrchestratorData.ComputeResourceEventCreated.builder()
                    .resource(vSphereHttpClient.vmIdAsUri(instance)).node(request.getNode()).build();
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
        throw new PersephoneException("Unable to power off VM {}", request.getResource());
    }

    @Override
    public Flow.Publisher<OrchestratorData.NetworkResourceEvent> createNetworkAddress(
            OrchestratorData.CreateNetworkResourceRequest request) {
        return publisher -> {
            val network = info.getVsphere().getNetwork();

            val privateIpAddress = ipamClient.allocatedPrivateIp(network.getName());

            if (privateIpAddress != null) {
                log.info("Assigned private IP {}", privateIpAddress);

                val event = OrchestratorData.NetworkResourceEventCreated.builder()
                        .name(request.getName())
                        .address(NetworkAddress.toIPv4Address(privateIpAddress.getValue()))
                        .publicResource(false)
                        .build();

                event.setResource(
                        URI.create("/" + network.getAllocationServer().getAddress() + "/" + network.getName()));

                publisher.onNext(event);

                // If request asked for public IP provisioning as well, send an event.
                if (request.getPublicResource()) {
                    val publicAddressEvent = OrchestratorData.NetworkResourceEventCreated.builder()
                            .name(request.getName())
                            .address(NetworkAddress.toIPv4Address(privateIpAddress.getValue()))
                            .publicResource(true)
                            .build();
                    publicAddressEvent.setResource(
                            URI.create("/" + network.getAllocationServer().getAddress() + "/" + network.getName()));
                    publisher.onNext(publicAddressEvent);
                }
                publisher.onComplete();
            }
        };
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
/*
 * Copyright (c) 2020 VMware, Inc. All rights reserved. VMware Confidential
 */

package com.vmware.blockchain.deployment.services.orchestration.vmc;

import java.net.InetAddress;
import java.net.URI;
import java.util.Map;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.Flow;

import com.google.common.net.InetAddresses;
import com.vmware.blockchain.deployment.services.exception.PersephoneException;
import com.vmware.blockchain.deployment.services.orchestration.NetworkAddress;
import com.vmware.blockchain.deployment.services.orchestration.Orchestrator;
import com.vmware.blockchain.deployment.services.orchestration.OrchestratorData;
import com.vmware.blockchain.deployment.services.orchestration.ipam.IpamClient;
import com.vmware.blockchain.deployment.services.orchestration.model.nsx.NsxData;
import com.vmware.blockchain.deployment.services.orchestration.model.vmc.VmcOnAwsData;
import com.vmware.blockchain.deployment.services.orchestration.vm.CloudInitConfiguration;
import com.vmware.blockchain.deployment.services.orchestration.vsphere.VSphereHttpClient;
import com.vmware.blockchain.deployment.v1.OutboundProxyInfo;
import com.vmware.blockchain.deployment.v1.VmcOrchestrationSiteInfo;

import lombok.extern.slf4j.Slf4j;
import lombok.val;

/**
 * Deployment orchestration driver for VMware Cloud [orchestration site type]
 * [OrchestrationSiteInfo.Type.VMC].
 */
@Slf4j
public class VmcOrchestrator implements Orchestrator {

    /**
     * Constructor.
     */
    public VmcOrchestrator(VmcOrchestrationSiteInfo info, IpamClient ipamClient) {

        this.info = info;
        this.ipamClient = ipamClient;
        try {
            // Create new VMC client.
            val vmcContext = VmcHttpClient.Context.builder()
                    .endpoint(URI.create(info.getApi().getAddress()))
                    .authenticationEndpoint(URI.create(info.getAuthentication().getAddress()))
                    .refreshToken(info.getAuthentication().getCredential().getTokenCredential().getToken())
                    .organization(info.getOrganization())
                    .datacenter(info.getDatacenter())
                    .build();

            this.vmc = new VmcHttpClient(vmcContext);

            log.info("Fetching Sddc info: " + vmcContext.datacenter);
            VmcOnAwsData.Sddc sddc = vmc.getDataCenterInfo();

            val nsxContext = VmcHttpClient.Context.builder()
                    .endpoint(URI.create(sddc.getResourceConfig().getNsxApiPublicEndpointUrl()))
                    .authenticationEndpoint(URI.create(info.getAuthentication().getAddress()))
                    .refreshToken(info.getAuthentication().getCredential().getTokenCredential().getToken())
                    .organization(info.getOrganization())
                    .datacenter(info.getDatacenter())
                    .build();
            nsx = new VmcHttpClient(nsxContext);

            VSphereHttpClient.Context context = new VSphereHttpClient.Context(
                    URI.create(sddc.getResourceConfig().getVcUrl()),
                    sddc.getResourceConfig().getCloudUsername(),
                    sddc.getResourceConfig().getCloudPassword());
            vSphereHttpClient = new VSphereHttpClient(context);

        } catch (Exception e) {
            throw new PersephoneException(e, "Error creating VMC orchestrator.");
        }
    }

    private VmcOrchestrationSiteInfo info;
    private VmcHttpClient vmc;
    private VmcHttpClient nsx;
    private VSphereHttpClient vSphereHttpClient;
    private IpamClient ipamClient;

    @Override
    public void initialize() {

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
                                    OutboundProxyInfo.newBuilder().build()
                                );

            val instance = vSphereHttpClient
                    .createVirtualMachine(request.getCluster().getId() + "-" + request.getNode().getId(),
                                          libraryItem, datastore, resourcePool, folder,
                                          Map.entry("blockchain-network", controlNetwork), cloudInit);

            vSphereHttpClient.ensureVirtualMachinePowerStart(instance, 500L);
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
            val privateIpAddress = ipamClient.allocatedPrivateIp(info.getDatacenter() + "-"
                                                                 + info.getVsphere().getNetwork().getName());

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

            val publicIpAddress = nsx.createPublicIp(request.getName());

            if (publicIpAddress != null) {
                log.info("Created public IP {}", publicIpAddress.getIp());

                val event = OrchestratorData.NetworkResourceEventCreated.builder()
                        .name(request.getName())
                        .address(publicIpAddress.getIp())
                        .publicResource(true)
                        .resource(URI.create(nsx.resolvePublicIpIdentifier(publicIpAddress.getId())))
                        .build();

                publisher.onNext(event);
            }
            publisher.onComplete();
        };
    }

    @Override
    public Flow.Publisher<OrchestratorData.NetworkResourceEvent> deleteNetworkAddress(
            OrchestratorData.DeleteNetworkResourceRequest request) {
        return publisher -> {
            if (request.getResource().getScheme() == null) {
                if (ipamClient.releasePrivateIp(request.getResource())) {
                    log.info("Released private IP resource {}", request.getResource());

                    val event = new OrchestratorData.NetworkResourceEvent(request.getResource());
                    publisher.onNext(event);
                    publisher.onComplete();
                } else {
                    publisher
                            .onError(new PersephoneException("Unable to delete network address {}",
                                                             request.getResource()));
                }
            } else {
                if (nsx.deleteResource(request.getResource())) {
                    log.info("Released public IP resource {}", request.getResource());

                    val event = new OrchestratorData.NetworkResourceEvent(request.getResource());
                    publisher.onNext(event);
                    publisher.onComplete();
                } else {
                    publisher
                            .onError(new PersephoneException("Unable to delete network address {}",
                                                             request.getResource()));
                }
            }
        };
    }

    @Override
    public Flow.Publisher<OrchestratorData.NetworkAllocationEvent> createNetworkAllocation(
            OrchestratorData.CreateNetworkAllocationRequest request) {
        return publisher -> {
            val privateIpString = request.getPrivateNetwork().getPath();
            val privateHex = privateIpString.substring(privateIpString.lastIndexOf("/") + 1);
            val privateIp = NetworkAddress.convertHexToIp(privateHex);

            val networkIdString = request.getPublicNetwork().getPath();
            val networkId = networkIdString.substring(networkIdString.lastIndexOf("/") + 1);

            val publicIp = nsx.getPublicIp(networkId).getIp();
            val rule = nsx.createNatRule("cgw", "USER", request.getName(), NsxData.NatRule.Action.REFLEXIVE,
                                         privateIp, null, publicIp, null, 3, 20L);

            val response = nsx.resolveNsxResourceIdentifier(rule.getPath());
            OrchestratorData.NetworkAllocationEventCreated event =
                    OrchestratorData.NetworkAllocationEventCreated.builder()
                            .name(request.getName())
                            .compute(request.getCompute())
                            .publicNetwork(request.getPublicNetwork())
                            .resource(response)
                            .privateNetwork(request.getPrivateNetwork()).build();

            publisher.onNext(event);
            publisher.onComplete();
        };
    }

    @Override
    public Flow.Publisher<OrchestratorData.NetworkAllocationEvent> deleteNetworkAllocation(
            OrchestratorData.DeleteNetworkAllocationRequest request) {

        return publisher -> {
            OrchestratorData.NetworkAllocationEvent event = new OrchestratorData.NetworkAllocationEvent();
            if (nsx.deleteResource(request.getResource())) {
                event.setResource(request.getResource());
            }
            publisher.onNext(event);
            publisher.onComplete();
        };
    }
}
/*
 * Copyright (c) 2020 VMware, Inc. All rights reserved. VMware Confidential
 */

package com.vmware.blockchain.deployment.services.orchestration.vsphere;

import java.net.InetAddress;
import java.net.URI;
import java.net.UnknownHostException;
import java.text.MessageFormat;
import java.util.AbstractMap;
import java.util.Map;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.ExecutionException;
import java.util.concurrent.Flow;

import com.vmware.blockchain.deployment.services.exception.BadRequestPersephoneException;
import com.vmware.blockchain.deployment.services.exception.ErrorCode;
import com.vmware.blockchain.deployment.services.exception.NotFoundPersephoneException;
import com.vmware.blockchain.deployment.services.exception.PersephoneException;
import com.vmware.blockchain.deployment.services.orchestration.Orchestrator;
import com.vmware.blockchain.deployment.services.orchestration.OrchestratorData;
import com.vmware.blockchain.deployment.services.orchestration.OrchestratorSiteInformation;
import com.vmware.blockchain.deployment.services.orchestration.OrchestratorUtils;
import com.vmware.blockchain.deployment.services.orchestration.ipam.IpamClient;
import com.vmware.blockchain.deployment.services.orchestration.vm.CloudInitConfiguration;
import com.vmware.blockchain.deployment.services.util.password.PasswordGeneratorUtil;
import com.vmware.blockchain.deployment.v1.ConcordClusterIdentifier;
import com.vmware.blockchain.deployment.v1.DeploymentAttributes;
import com.vmware.blockchain.deployment.v1.Endpoint;
import com.vmware.blockchain.deployment.v1.VSphereDatacenterInfo;

import lombok.extern.slf4j.Slf4j;
import lombok.val;

/**
 * Deployment orchestration driver for VMware vSphere environment.
 */
@Slf4j
public class VSphereOrchestrator implements Orchestrator {

    private VSphereDatacenterInfo datacenterInfo;
    private IpamClient ipamClient;
    private VSphereHttpClient vSphereHttpClient;

    private OrchestratorSiteInformation orchestratorSiteInformation;

    /**
     * Constructor.
     */
    public VSphereOrchestrator(VSphereDatacenterInfo info,
                               Endpoint vCenter,
                               IpamClient ipamClient) {
        this.datacenterInfo = info;
        this.ipamClient = ipamClient;

        // Create new vSphere client.
        VSphereHttpClient.Context context = new VSphereHttpClient.Context(
                URI.create(vCenter.getAddress()),
                vCenter.getCredential().getPasswordCredential().getUsername(),
                vCenter.getCredential().getPasswordCredential().getPassword());

        this.vSphereHttpClient = new VSphereHttpClient(context);
    }

    @Override
    public void populate() {
        val compute = datacenterInfo.getResourcePool();
        val storage = datacenterInfo.getDatastore();
        val network = datacenterInfo.getNetwork();

        val getFolder = CompletableFuture.supplyAsync(() -> vSphereHttpClient.getFolder(datacenterInfo.getFolder()));
        val getDatastore = CompletableFuture.supplyAsync(() -> vSphereHttpClient.getDatastore(storage));
        val getResourcePool = CompletableFuture.supplyAsync(() -> vSphereHttpClient.getResourcePool(compute));
        val getControlNetwork = CompletableFuture.supplyAsync(() -> vSphereHttpClient.getNetwork(network.getName()));


        orchestratorSiteInformation = new OrchestratorSiteInformation();
        try {
            orchestratorSiteInformation.setNetwork(getControlNetwork.get());
        } catch (InterruptedException | ExecutionException e) {
            log.error(MessageFormat.format(ErrorCode.SITE_NETWORK_INCORRECT, network.getName()));
            throw new BadRequestPersephoneException(e, ErrorCode.SITE_NETWORK_INCORRECT, network.getName());
        }
        try {
            orchestratorSiteInformation.setDataStore(getDatastore.get());
        } catch (InterruptedException | ExecutionException e) {
            log.error(MessageFormat.format(ErrorCode.SITE_DATASTORE_INCORRECT, storage));
            throw new BadRequestPersephoneException(e, ErrorCode.SITE_DATASTORE_INCORRECT, storage);
        }
        try {
            orchestratorSiteInformation.setResourcePool(getResourcePool.get());
        } catch (InterruptedException | ExecutionException e) {
            log.error(MessageFormat.format(ErrorCode.SITE_RESOURCE_POOL_INCORRECT, compute));
            throw new BadRequestPersephoneException(e, ErrorCode.SITE_RESOURCE_POOL_INCORRECT, compute);
        }
        try {
            orchestratorSiteInformation.setFolder(getFolder.get());
        } catch (InterruptedException | ExecutionException e) {
            log.error(MessageFormat.format(ErrorCode.SITE_FOLDER_INCORRECT, datacenterInfo.getFolder()));
            throw new BadRequestPersephoneException(e, ErrorCode.SITE_FOLDER_INCORRECT, datacenterInfo.getFolder());
        }
    }

    @Override
    public OrchestratorData.ComputeResourceEventCreatedV2 createDeploymentV2(
            OrchestratorData.CreateComputeResourceRequestV2 request) {

        try {

            val getLibraryItem =
                    CompletableFuture
                            .supplyAsync(() -> vSphereHttpClient.getLibraryItem(request.getCloudInitData()
                                                                                        .getModel().getTemplate()));

            var vmPassword = request.getProperties().containsKey(DeploymentAttributes.GENERATE_PASSWORD.name())
                             ? PasswordGeneratorUtil.generateCommonTextPassword() : "c0nc0rd";

            val cloudInit = new CloudInitConfiguration(
                    request.getCloudInitData().getContainerRegistry(),
                    request.getCloudInitData().getModel(),
                    request.getCloudInitData().getPrivateIp(),
                    InetAddress.getByName(String.valueOf(datacenterInfo.getNetwork().getGateway()))
                            .getHostAddress(),
                    datacenterInfo.getNetwork().getNameServersList(),
                    datacenterInfo.getNetwork().getSubnet(),
                    ConcordClusterIdentifier.newBuilder()
                            .setId(request.getBlockchainId().toString())
                            .build(),
                    request.getNodeId().toString(),
                    request.getCloudInitData().getConfigGenId(),
                    request.getCloudInitData().getConfigServiceRestEndpoint(),
                    datacenterInfo.getOutboundProxy(),
                    vmPassword,
                    request.getProperties().containsKey(DeploymentAttributes.NO_LAUNCH.name()),
                    request.getProperties().containsKey(DeploymentAttributes.VM_STORAGE.name())
            );

            var libraryItem = getLibraryItem.get();
            var instance = vSphereHttpClient
                    .createVirtualMachine(request.getVmId(),
                                          libraryItem, orchestratorSiteInformation.getDataStore(),
                                          orchestratorSiteInformation.getResourcePool(),
                                          orchestratorSiteInformation.getFolder(),
                                          Map.entry("blockchain-network", orchestratorSiteInformation.getNetwork()),
                                          cloudInit);

            // Temp for now
            vSphereHttpClient.ensureVirtualMachinePowerStart(instance, 5000L, request.getProperties());
            return OrchestratorData.ComputeResourceEventCreatedV2.builder()
                    .resource(vSphereHttpClient.vmIdAsUri(instance))
                    .password(vmPassword)
                    .nodeId(request.getNodeId()).build();

        } catch (UnknownHostException e) {
            throw new NotFoundPersephoneException(e, ErrorCode.UNKNOWN_GATEWAY,
                    datacenterInfo.getNetwork().getGateway());
        } catch (InterruptedException | ExecutionException e) {
            throw new NotFoundPersephoneException(e, ErrorCode.NOT_FOUND_LIBRARY_ITEM,
                    request.getCloudInitData().getModel().getTemplate());
        } catch (Exception e) {
            throw new PersephoneException(e, ErrorCode.VM_START_ERROR);
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
        throw new PersephoneException(ErrorCode.VM_POWER_OFF_ERROR, request.getResource());
    }

    @Override
    public OrchestratorData.NetworkResourceEvent createPrivateNetworkAddress(
            OrchestratorData.CreateNetworkResourceRequest request) {

        AbstractMap.SimpleEntry<String, String> privateIpAddress = OrchestratorUtils.getAddress(ipamClient,
                datacenterInfo.getNetwork().getName(),
                request.getIp(), request.getName());
        log.info("Assigned private IP {}", privateIpAddress);

        return OrchestratorData.NetworkResourceEventCreated.builder()
                .name(request.getName())
                .address(privateIpAddress.getValue())
                .publicResource(false)
                .resource(URI.create("/" + privateIpAddress.getKey()))
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
                publisher.onError(new PersephoneException(ErrorCode.RESOURCE_NETWORK_DELETION_FAILURE,
                        request.getResource()));
            }
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

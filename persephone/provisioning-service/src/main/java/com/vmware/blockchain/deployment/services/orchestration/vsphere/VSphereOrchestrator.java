/*
 * Copyright (c) 2020 VMware, Inc. All rights reserved. VMware Confidential
 */

package com.vmware.blockchain.deployment.services.orchestration.vsphere;

import java.net.URI;
import java.text.MessageFormat;
import java.util.AbstractMap;
import java.util.Map;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.ExecutionException;
import java.util.concurrent.Flow;

import org.springframework.http.HttpStatus;
import org.springframework.web.client.HttpClientErrorException;

import com.vmware.blockchain.deployment.services.exception.BadRequestPersephoneException;
import com.vmware.blockchain.deployment.services.exception.ErrorCode;
import com.vmware.blockchain.deployment.services.exception.InternalFailurePersephoneException;
import com.vmware.blockchain.deployment.services.exception.NotFoundPersephoneException;
import com.vmware.blockchain.deployment.services.exception.PersephoneException;
import com.vmware.blockchain.deployment.services.orchestration.Orchestrator;
import com.vmware.blockchain.deployment.services.orchestration.OrchestratorData;
import com.vmware.blockchain.deployment.services.orchestration.OrchestratorSiteInformation;
import com.vmware.blockchain.deployment.services.orchestration.OrchestratorUtils;
import com.vmware.blockchain.deployment.services.orchestration.ipam.IpamClient;
import com.vmware.blockchain.deployment.services.orchestration.vm.CloudInitConfiguration;
import com.vmware.blockchain.deployment.services.util.password.PasswordGeneratorUtil;
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
    private String vCenterUrl;

    private OrchestratorSiteInformation orchestratorSiteInformation;

    /**
     * Constructor.
     */
    public VSphereOrchestrator(VSphereDatacenterInfo info,
                               Endpoint vCenter,
                               IpamClient ipamClient) {
        this.datacenterInfo = info;
        this.ipamClient = ipamClient;
        this.vCenterUrl = vCenter.getAddress();

        // Create new vSphere client.
        VSphereHttpClient.Context context = new VSphereHttpClient.Context(
                URI.create(vCenter.getAddress()),
                vCenter.getCredential().getPasswordCredential().getUsername(),
                vCenter.getCredential().getPasswordCredential().getPassword(),
                vCenter.getTransportSecurity().getCertificateData());

        this.vSphereHttpClient = new VSphereHttpClient(context);
    }

    @Override
    public void populate() {

        // First ensure that the server is functional, otherwise there is no point in checking further.
        try {
            HttpStatus healthStatus = vSphereHttpClient.getHealth();
        } catch (HttpClientErrorException.Unauthorized e) {
            log.error("Server credentials invalid: {}", vCenterUrl);
            throw new BadRequestPersephoneException(ErrorCode.INVALID_CREDENTIALS, vCenterUrl);
        } catch (InternalFailurePersephoneException e) {
            // This exception is thrown when the server is unreachable.
            if (e.getHttpStatus() == HttpStatus.INTERNAL_SERVER_ERROR) {
                log.error("Unreachable vCenter: {}", e);
                throw new BadRequestPersephoneException(ErrorCode.SERVER_NOT_FUNCTIONAL, vCenterUrl);
            } else {
                log.error("Unknown error: {}", e);
                throw new BadRequestPersephoneException(ErrorCode.REQUEST_EXECUTION_FAILURE, vCenterUrl);
            }
        } catch (Exception e) {
            log.error("Unknown error: {}", e);
            throw new BadRequestPersephoneException(ErrorCode.REQUEST_EXECUTION_FAILURE, vCenterUrl);
        }

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
                             ? PasswordGeneratorUtil.generateCommonTextPassword() : "Bl0ckch@!n";

            val cloudInit = new CloudInitConfiguration(request, datacenterInfo, vmPassword);

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

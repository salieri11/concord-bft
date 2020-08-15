/*
 * Copyright (c) 2020 VMware, Inc. All rights reserved. VMware Confidential
 */

package com.vmware.blockchain.deployment.services.orchestration.vmc;

import java.net.URI;
import java.util.AbstractMap;
import java.util.concurrent.Flow;

import com.vmware.blockchain.deployment.services.exception.ErrorCode;
import com.vmware.blockchain.deployment.services.exception.PersephoneException;
import com.vmware.blockchain.deployment.services.orchestration.Orchestrator;
import com.vmware.blockchain.deployment.services.orchestration.OrchestratorData;
import com.vmware.blockchain.deployment.services.orchestration.OrchestratorUtils;
import com.vmware.blockchain.deployment.services.orchestration.ipam.IpamClient;
import com.vmware.blockchain.deployment.services.orchestration.model.nsx.NsxData;
import com.vmware.blockchain.deployment.services.orchestration.model.vmc.VmcOnAwsData;
import com.vmware.blockchain.deployment.services.orchestration.vsphere.VSphereOrchestrator;
import com.vmware.blockchain.deployment.v1.Credential;
import com.vmware.blockchain.deployment.v1.Endpoint;
import com.vmware.blockchain.deployment.v1.PasswordCredential;
import com.vmware.blockchain.deployment.v1.VmcOrchestrationSiteInfo;

import lombok.extern.slf4j.Slf4j;
import lombok.val;

/**
 * Deployment orchestration driver for VMware Cloud [orchestration site type]
 * [OrchestrationSiteInfo.Type.VMC].
 */
@Slf4j
public class VmcOrchestrator implements Orchestrator {


    private VmcOrchestrationSiteInfo info;
    private VmcHttpClient vmc;
    private VmcHttpClient nsx;
    private IpamClient ipamClient;

    private VSphereOrchestrator internalOrchestrator;

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

            Endpoint vsphereEndpoint = Endpoint.newBuilder()
                    .setAddress(sddc.getResourceConfig().getVcUrl())
                    .setCredential(Credential.newBuilder()
                            .setPasswordCredential(PasswordCredential.newBuilder()
                                    .setUsername(sddc.getResourceConfig().getCloudUsername())
                                    .setPassword(sddc.getResourceConfig().getCloudPassword())
                                    .build())
                            .build())
                    .build();
            internalOrchestrator = new VSphereOrchestrator(info.getVsphere(), vsphereEndpoint, ipamClient);
        } catch (Exception e) {
            throw new PersephoneException(e, ErrorCode.VMC_ORCHESTRATION_CREATION_FAILURE);
        }
    }

    @Override
    public void populate() {
        internalOrchestrator.populate();
    }

    @Override
    public OrchestratorData.ComputeResourceEventCreatedV2 createDeploymentV2(
            OrchestratorData.CreateComputeResourceRequestV2 request) {
        return internalOrchestrator.createDeploymentV2(request);
    }

    @Override
    public OrchestratorData.ComputeResourceEvent deleteDeployment(
            OrchestratorData.DeleteComputeResourceRequest request) {
        return internalOrchestrator.deleteDeployment(request);
    }

    @Override
    public OrchestratorData.NetworkResourceEvent createPrivateNetworkAddress(
            OrchestratorData.CreateNetworkResourceRequest request) {

        AbstractMap.SimpleEntry<String, String> privateIpAddress = OrchestratorUtils.getAddress(ipamClient,
                info.getDatacenter() + "-" + info.getVsphere().getNetwork().getName(),
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
    public OrchestratorData.NetworkResourceEvent createPublicNetworkAddress(
            OrchestratorData.CreateNetworkResourceRequest request) {

        val publicIpAddress = nsx.createPublicIp(request.getName());
        log.info("Created public IP {}", publicIpAddress.getIp());

        return OrchestratorData.NetworkResourceEventCreated.builder()
                .name(request.getName())
                .address(publicIpAddress.getIp())
                .publicResource(true)
                .resource(URI.create(nsx.resolvePublicIpIdentifier(publicIpAddress.getId())))
                .build();
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
                    publisher.onError(new PersephoneException(ErrorCode.RESOURCE_NETWORK_DELETION_FAILURE,
                            request.getResource()));
                }
            } else {
                if (nsx.deleteResource(request.getResource())) {
                    log.info("Released public IP resource {}", request.getResource());

                    val event = new OrchestratorData.NetworkResourceEvent(request.getResource());
                    publisher.onNext(event);
                    publisher.onComplete();
                } else {
                    publisher.onError(new PersephoneException(ErrorCode.RESOURCE_NETWORK_DELETION_FAILURE,
                            request.getResource()));
                }
            }
        };
    }

    @Override
    public OrchestratorData.NetworkAllocationEvent createVmcNetworkAllocation(
            OrchestratorData.CreateNetworkAllocationRequestV2 request) {

        val rule = nsx.createNatRule("cgw", "USER", request.getName(), NsxData.NatRule.Action.REFLEXIVE,
                                     request.getPrivateIp(), null, request.getPublicIp(), null, 3, 20L);

        val response = nsx.resolveNsxResourceIdentifier(rule.getPath());
        return new OrchestratorData.NetworkAllocationEvent(response);
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
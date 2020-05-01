/*
 * Copyright (c) 2020 VMware, Inc. All rights reserved. VMware Confidential
 */

package com.vmware.blockchain.deployment.services.orchestration.ipam;

import java.net.URI;
import java.util.concurrent.CompletableFuture;

import com.vmware.blockchain.deployment.services.exception.PersephoneException;
import com.vmware.blockchain.deployment.services.futureutil.ReactiveStream;
import com.vmware.blockchain.deployment.v1.Address;
import com.vmware.blockchain.deployment.v1.AllocateAddressRequest;
import com.vmware.blockchain.deployment.v1.AllocateAddressResponse;
import com.vmware.blockchain.deployment.v1.Endpoint;
import com.vmware.blockchain.deployment.v1.IPAllocationServiceGrpc;
import com.vmware.blockchain.deployment.v1.MessageHeader;
import com.vmware.blockchain.deployment.v1.ReleaseAddressRequest;
import com.vmware.blockchain.deployment.v1.ReleaseAddressResponse;
import com.vmware.blockchain.deployment.v1.TransportSecurity;

import io.grpc.ManagedChannelBuilder;
import lombok.extern.slf4j.Slf4j;
import lombok.val;

/**
 * IPAM client.
 */
@Slf4j
public class IpamClient {

    private static final String IPAM_RESOURCE_NAME_PREFIX = "blocks/";
    private final IPAllocationServiceGrpc.IPAllocationServiceStub ipAllocationServiceStub;

    /**
     * Constructor.
     */
    public IpamClient(Endpoint allocationServer) {
        ManagedChannelBuilder managedChannelBuilder = ManagedChannelBuilder.forTarget(allocationServer.getAddress());

        if (allocationServer.getTransportSecurity().getType() == TransportSecurity.Type.NONE) {
            managedChannelBuilder.usePlaintext();
        }

        ipAllocationServiceStub = IPAllocationServiceGrpc.newStub(managedChannelBuilder.build());
    }

    /**
     * Allocate a new private IP address for use from IP allocation service.
     *
     * @param networkSegmentName network to allocate IP address resource from.
     * @return allocated IP address resource as a instance of [Address], if created, `null` otherwise.
     */
    public Address allocatedPrivateIp(String networkSegmentName) {
        val requestAllocateAddress = AllocateAddressRequest.newBuilder()
                .setHeader(MessageHeader.newBuilder().build())
                .setParent(IPAM_RESOURCE_NAME_PREFIX + networkSegmentName).build();

        var promise = new CompletableFuture<AllocateAddressResponse>();
        try {
            // Added retry to mitigate intermittent gRPC timeout. Make it more precise.
            int retry = 3;
            boolean success = false;
            do {
                try {
                    ipAllocationServiceStub.allocateAddress(requestAllocateAddress,
                                                            ReactiveStream.blockedResultObserver(promise));
                    success = true;
                } catch (Throwable e) {
                    log.warn("Error received from IPAM service", e);
                    retry--;
                }
            } while (!success && retry > 0);

            Address address = promise.get().getAddress();
            // Hack to adjust with IPAM response.
            if (address.getValue() <= 0) {
                throw new PersephoneException("No IP was allocated.");
            }
            log.info("Assigned ip {}", address.getValue());
            return address;
        } catch (Exception e) {
            throw new PersephoneException(e, "Error Allocating ip for segment:" + networkSegmentName);
        }
    }

    /**
     * Release an IP address resource from a given [IPv4Network].
     *
     * @param resource URI of the resource to be released.
     * @return `true` if resource is successfully released, `false` otherwise.
     */
    public boolean releasePrivateIp(URI resource) {
        val requestAllocateAddress = ReleaseAddressRequest.newBuilder().setHeader(MessageHeader.newBuilder().build())
                .setName(resource.getPath().replaceFirst("/", "")).build();
        try {
            var promise = new CompletableFuture<ReleaseAddressResponse>();
            ipAllocationServiceStub.releaseAddress(requestAllocateAddress,
                                                   ReactiveStream.blockedResultObserver(promise));
            return true;
        } catch (Exception e) {
            throw new PersephoneException(e, "Error releasing private ip: " +  resource);
        }
    }

}

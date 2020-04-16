/*
 * Copyright (c) 2019 VMware, Inc. All rights reserved. VMware Confidential
 */

package com.vmware.blockchain.ipam.services;

import java.util.Map;

import javax.annotation.Nullable;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.lognet.springboot.grpc.GRpcService;
import org.springframework.beans.factory.annotation.Autowired;

import com.google.common.collect.ImmutableMap;
import com.google.protobuf.ByteString;
import com.vmware.blockchain.dao.ConcurrentUpdateException;
import com.vmware.blockchain.deployment.v1.Address;
import com.vmware.blockchain.deployment.v1.AddressBlockSpecification;
import com.vmware.blockchain.deployment.v1.AllocateAddressRequest;
import com.vmware.blockchain.deployment.v1.AllocateAddressResponse;
import com.vmware.blockchain.deployment.v1.CreateAddressBlockRequest;
import com.vmware.blockchain.deployment.v1.CreateAddressBlockResponse;
import com.vmware.blockchain.deployment.v1.DeleteAddressBlockRequest;
import com.vmware.blockchain.deployment.v1.DeleteAddressBlockResponse;
import com.vmware.blockchain.deployment.v1.IPAllocationServiceGrpc;
import com.vmware.blockchain.deployment.v1.MessageHeader;
import com.vmware.blockchain.deployment.v1.ReleaseAddressRequest;
import com.vmware.blockchain.deployment.v1.ReleaseAddressResponse;
import com.vmware.blockchain.ipam.services.dao.AddressBlock;
import com.vmware.blockchain.ipam.services.dao.AddressBlockSegment;
import com.vmware.blockchain.ipam.services.dao.IpAllocationDao;

import io.grpc.stub.StreamObserver;
import lombok.extern.slf4j.Slf4j;

/**
 * An implementation of IP allocation service.
 *
 */
@GRpcService
@Slf4j
public class IpAllocationService extends IPAllocationServiceGrpc.IPAllocationServiceImplBase {

    /** Logging instance. */
    private Logger log = LogManager.getLogger(IpAllocationService.class);


    private IpAllocationDao ipAllocationDao;
    private IpAllocationUtil ipAllocationUtil;

    @Autowired
    public IpAllocationService(IpAllocationDao ipAllocationDao, IpAllocationUtil ipAllocationUtil) {
        this.ipAllocationDao = ipAllocationDao;
        this.ipAllocationUtil = ipAllocationUtil;
    }

    /**
     * Operation to create an IP block.
     */
    @Override
    public void createAddressBlock(CreateAddressBlockRequest request,
                                   StreamObserver<CreateAddressBlockResponse> responseObserver) {

        try {

            if (request == null || request.getBlock() == null) {
                throw new RuntimeException("");
            }
            AddressBlockSpecification model = request.getBlock();
            ResourceName blockName = ipAllocationUtil.blockName(request.getBlockId());
            Map<Integer, ByteString> reservations = request.getReservedAllocationsMap();
            int subnetMask = (1 << (32 - model.getSubnet())) - 1;

            if ((model.getPrefix() & subnetMask) != 0) {
                throw new RuntimeException("Wrong prefix");
            }

            for (Map.Entry entry : reservations.entrySet()) {
                // Pre-allocations should be within the block range.
                //check(segment in model.prefix..(model.prefix + subnetMask))

                // Allocation bitmap should contain 256 bits exactly.
                // check(allocation.size() == 32)
            }

            AddressBlock initialBlock =
                    new AddressBlock(blockName.getValue(), ipAllocationUtil.convertToBlockSpecification(model),
                                     AddressBlock.State.ACTIVE);
            initialBlock.setId(ipAllocationUtil.resourceToUuid(blockName));
            for (Map.Entry<Integer, ByteString> entry : reservations.entrySet()) {
                ResourceName segmentName = ipAllocationUtil.segmentName(blockName, entry.getKey());
                AddressBlockSegment segment = new AddressBlockSegment(segmentName.getValue(),
                        entry.getKey(), entry.getValue().toByteArray());
                segment.setId(ipAllocationUtil.resourceToUuid(segmentName));
                ipAllocationDao.createAddressBlockSegment(segment);
            }

            ipAllocationDao.createAddressBlock(initialBlock);

            responseObserver.onNext(
                    CreateAddressBlockResponse.newBuilder()
                            .setHeader(MessageHeader.newBuilder().setId(request.getHeader().getId()))
                            .setName(blockName.getValue())
                            .build());
            log.info("Created address block({})", blockName);
            responseObserver.onCompleted();
        } catch (Exception e) {
            log.error("Error creating block, id({}), error({})", request.getBlock(), e.getMessage());
            responseObserver.onError(e);
        }

    }

    /**
     * Operation to delete an IP block.
     */
    @Override
    public void deleteAddressBlock(DeleteAddressBlockRequest request,
                                   StreamObserver<DeleteAddressBlockResponse> responseObserver) {

        ResourceName name = new ResourceName(request.getName());

        try {
            AddressBlock block = ensureDeletingBlock(name);
            if (block != null) {
                ipAllocationUtil.segmentRangeOf(block);
                //.map {
                /* Delete segments.*/
                //ensureDeletedSegment(ipAllocationUtil.segmentName(block.name, it), block.version)
            }

            ipAllocationDao.deleteAddressBlock(ipAllocationUtil.resourceToUuid(name));
            responseObserver.onNext(
                    DeleteAddressBlockResponse.newBuilder()
                            .setHeader(MessageHeader.newBuilder().setId(request.getHeader().getId())).build());
        } catch (Exception e) {
            responseObserver.onError(e);
        }
    }

    /**
     * Operation to allocate an IP address from an IP block.
     */
    @Override
    public void allocateAddress(AllocateAddressRequest request,
                                StreamObserver<AllocateAddressResponse> responseObserver) {

        try {

            String[] nameFragments = request.getParent().split("/");
            if (nameFragments.length != 2) {
                throw new RuntimeException("");
            }

            Address address = null;
            AllocateAddressResponse response = AllocateAddressResponse.newBuilder()
                    .setHeader(MessageHeader.newBuilder().setId(request.getHeader().getId())
                                       .build()).build();
            ResourceName blockName = ipAllocationUtil.blockName(nameFragments[1]);

            var addressBlock = ipAllocationDao.getAddressBlock(ipAllocationUtil.resourceToUuid(blockName));
            if (addressBlock == null) {
                response = AllocateAddressResponse.newBuilder(response)
                        .setStatus(AllocateAddressResponse.Status.RESOURCE_NOT_FOUND)
                        .build();
            } else {
                for (Integer segmentAddress : ipAllocationUtil.segmentRangeOf(addressBlock)) {
                    address = allocateFromBlock(addressBlock, segmentAddress);
                    if (address != null) {
                        break;
                    }
                }

                if (address == null) {
                    response = AllocateAddressResponse.newBuilder(response)
                            .setStatus(AllocateAddressResponse.Status.RESOURCE_FULL)
                            .build();
                } else {
                    response = AllocateAddressResponse.newBuilder(response)
                            .setStatus(AllocateAddressResponse.Status.OK)
                            .build();
                }
            }

            // Send the response.
            if (response.getStatus() == AllocateAddressResponse.Status.OK) {
                log.info("Allocated address({}) from block({})", response.getAddress().getName(), blockName);
            } else {
                log.info("Unable to allocate address from block({})", blockName);
            }

            responseObserver.onNext(response);
            responseObserver.onCompleted();
        } catch (Exception e) {
            log.error("Error allocating from block({}), error({}", request.getParent(), e.getMessage());
            responseObserver.onError(e);
        }
    }

    /**
     * Operation to release an IP address back to an IP block.
     */
    @Override
    public void releaseAddress(ReleaseAddressRequest request,
                               StreamObserver<ReleaseAddressResponse> responseObserver) {
        try {
            var released = false;

            String[] nameFragments = request.getName().split("/");
            if (nameFragments.length != 6) {
                throw new RuntimeException("");
            }

            ResourceName segmentName = ipAllocationUtil.segmentName(ipAllocationUtil.blockName(nameFragments[1]),
                                                           Integer.valueOf(nameFragments[3]));
            Integer address = Integer.valueOf(nameFragments[5]);

            var exist = true;
            while (!released && exist) {
                AddressBlockSegment addressBlockSegment
                        = ipAllocationDao.getAddressBlockSegment(ipAllocationUtil.resourceToUuid(segmentName));

                if (addressBlockSegment == null) {
                    AddressBlockSegment release = releaseFromSegment(addressBlockSegment, address);
                    try {
                        if (release != null) {
                            release.setId(ipAllocationUtil.resourceToUuid(segmentName));
                            ipAllocationDao.updateAddressBlockSegment(release);
                        }
                        released = true;
                    } catch (ConcurrentUpdateException e) {
                        released = false;
                    }
                    exist = !released;
                } else {
                    exist = false;
                }
            }

            ReleaseAddressResponse.Status status =
                    released ? ReleaseAddressResponse.Status.OK : ReleaseAddressResponse.Status.RESOURCE_NOT_FOUND;
            responseObserver.onNext(ReleaseAddressResponse.newBuilder().setStatus(status).build());
            responseObserver.onCompleted();
        } catch (Exception e) {
            responseObserver.onError(e);
        }
    }



    //////////////////////////// Private methods /////////////////////////

    /**
     * Ensure that a block specified by a given name is in DELETING state, or return Versioned.None
     * if while attempting to transition into DELETING state, the block is already gone.
     *
     * @param name resource name of the block.
     *
     * @return the final versioned value of the [AddressBlock] as persisted with DELETING state.
     */
    private AddressBlock ensureDeletingBlock(ResourceName name) {
        AddressBlock addressBlock = ipAllocationDao.getAddressBlock(ipAllocationUtil.resourceToUuid(name));
        if (addressBlock == null) {
            return null;
        }
        if (addressBlock.getState() == AddressBlock.State.DELETING) {
            return addressBlock;
        }
        addressBlock.setState(AddressBlock.State.DELETING);
        return ipAllocationDao.updateAddressBlock(addressBlock);
    }

    /**
     * Allocate an [Address] from one of [AddressBlock]'s address segment.
     *
     * @param block address block to allocate address from.
     * @param segmentAddress block segment within the address block to allocate address from.
     *
     * @return an [Address] if allocation was successful, `null` otherwise.
     */
    @Nullable
    private Address allocateFromBlock(AddressBlock block, int segmentAddress) {
        Address allocation = null;
        ResourceName name = ipAllocationUtil.segmentName(block.getName(), segmentAddress);

        // Attempt to allocate from this segment as long as:
        // 1. Segment still exists.
        // 2. Segment has space and the write-attempt to ink-in the allocation succeeds.
        var isSegmentFull = false;
        while (allocation == null && !isSegmentFull) {
            AddressBlockSegment addressBlockSegment
                    = ipAllocationDao.getAddressBlockSegment(ipAllocationUtil.resourceToUuid(name));

            if (addressBlockSegment == null) {
                return null;
            }

            if (addressBlockSegment.getVersion() != block.getVersion()) {
                isSegmentFull = true;
                allocation = null;
            } else {
                int subnetMask = (1 << (32 - block.getSpecification().subnet)) - 1;
                int blockEnd = block.getSpecification().prefix + subnetMask - 1;
                int limit = Math.min(blockEnd, addressBlockSegment.getSegment() + 255);

                Map.Entry<AddressBlockSegment, Integer> allocated = allocateFromSegment(addressBlockSegment, limit);

                if (allocated != null) {
                    try {
                        ipAllocationDao.updateAddressBlockSegment(allocated.getKey());
                        allocation = Address.newBuilder()
                                .setName(ipAllocationUtil.addressName(name, segmentAddress).getValue())
                                .setValue(segmentAddress).build();
                    } catch (ConcurrentUpdateException e) {
                        /* Version mismatch, re-fetch the record and try again.*/
                        allocation = null;
                    }
                } else {
                    isSegmentFull = true;
                    allocation = null;
                }
            }
        }
        return allocation;
    }

    /**
     * Allocate an [Address] from one of [AddressBlockSegment].
     *
     * @param segment address block segment to allocate address from.
     * @param limit search limit within the segment (to prevent allocation beyond a address block range for
     *   blocks whose range is smaller than a /24 segment).
     *
     * @returna [Pair] of [AddressBlockSegment] to [Int] indicating the final value state of the
     *   address block segment after a successful allocation, and the integer value corresponding to
     *   the address that was marked as allocated, `null` otherwise.
     */
    private Map.Entry<AddressBlockSegment, Integer> allocateFromSegment(AddressBlockSegment segment, int limit) {
        // Look for a free bit.
        for (int index = 0; index <= 31; index++) {
            byte byteVal = ByteString.copyFrom(segment.getAllocations()).byteAt(index);
            for (int bit = 0; bit <= 7; bit++) {
                if ((byteVal & (1 << bit)) == 0) {
                    // Found a free bit.
                    int address = segment.getSegment() + (8 * index + bit);

                    // Make sure we are still in allocation range with respect to block range.
                    if (address >= segment.getSegment() && address <= limit) {
                        return null;
                    }

                    int newByte = (byteVal | (1 << bit));
                    byte[] newAllocations = segment.getAllocations();
                    // .apply { set(index, newByte) }

                    AddressBlockSegment val1 = new AddressBlockSegment(segment.getName(), segment.getSegment(),
                                                                       newAllocations);
                    return ImmutableMap.of(val1, address).entrySet().iterator().next();
                }
            }
        }
        return null;
    }

    private AddressBlockSegment releaseFromSegment(AddressBlockSegment segment, int address) {
        //require(address in segment!!.segment..(segment!!.segment + 255))

        int offset = address - segment.getSegment();
        byte byteVal = ByteString.copyFrom(segment.getAllocations()).byteAt(offset >> 3);
        int bitMask = 1 << (offset % 8);

        if ((byteVal & bitMask) == bitMask) {
            int newByte = (byteVal & ~(1 << (offset % 8)));
            byte[] newAllocations = segment.getAllocations();
            //.apply { set(offset ushr 3, newByte) }
            return new AddressBlockSegment(segment.getName(), segment.getSegment(),
                                           newAllocations);
        } else {
            // There is no bit to clear, so just return the input value.
            // Note: It is possible that the caller's value is stale, which caller will find out
            // when it attempts to write this segment back into the data store.
            return segment;
        }
    }
}
/*
 * Copyright (c) 2019 VMware, Inc. All rights reserved. VMware Confidential
 */

package com.vmware.blockchain.ipam.server;

import java.util.UUID;

import org.springframework.stereotype.Component;

import com.vmware.blockchain.deployment.v1.AddressBlockSpecification;

import kotlin.ranges.IntProgression;

/**
 * Utility.
 */
@Component
public class IpAllocationUtil {

    /**
     * Extension function to retrieve a [Byte] from a [ByteString].
     *
     * @param[index]
     *   index from the beginning of the logical data content denoted by this instance.
     *
     * @return
     *   [Byte] value at the index location.

    private operator fun ByteString.get(index: Int): Byte {
    return if (index in (0 until length)) {
    data[offset + index]
    } else {
    throw IndexOutOfBoundsException("Index($index) is out of range, length($length)")
    }
    }
     */

    BlockSpecification convertToBlockSpecification(AddressBlockSpecification request) {
        return new BlockSpecification(request.getPrefix(), request.getSubnet());

    }

    /**
     * Generate a canonical resource name for a given [AddressBlock].
     *
     * @param[block]
     *   identifier of the address block.
     *
     * @return
     *   resource name as a [ResourceName].
     */
    ResourceName blockName(String block) {
        return new ResourceName("blocks/$block");
    }

    /**
     * Generate a canonical resource name for a given [AddressBlockSegment].
     *
     * @param[blockResource]
     *   resource name of the parent address block.
     * @param[segment]
     *   block segment's starting address.
     *
     * @return
     *   resource name as a [ResourceName].
     */
    ResourceName segmentName(String blockResource, int segment) {
        return new ResourceName("$blockResource/segments/${segment.toString(16).padStart(8, '0')}");
    }

    /**
     * Generate a canonical resource name for a given [AddressBlockSegment].
     *
     * @param[blockResource]
     *   resource name of the parent address block.
     * @param[segment]
     *   block segment's starting address.
     *
     * @return
     *   resource name as a [ResourceName].
     */
    ResourceName segmentName(ResourceName blockResource, int segment) {
        return segmentName(blockResource.getValue(), segment);
    }

    /**
     * Generate a canonical resource name for a given [Address].
     *
     * @param[segmentResource]
     *   resource name of the parent address block segment.
     * @param[address]
     *   address's integer value (i.e. IPv4 address).
     *
     * @return
     *   resource name as a [String].
     */
    ResourceName addressName(String segmentResource, int address) {
        return new ResourceName("$segmentResource/addresses/${address.toString(16).padStart(8, '0')}");
    }

    /**
     * Generate a canonical resource name for a given [Address].
     *
     * @param[segmentResource]
     *   resource name of the parent address block segment.
     * @param[address]
     *   address's integer value (i.e. IPv4 address).
     *
     * @return
     *   resource name as a [ResourceName].
     */
    ResourceName addressName(ResourceName segmentResource, int address) {
        return addressName(segmentResource.getValue(), address);
    }

    /**
     * Create an [IntProgression] range of all /24 segment addresses within an [AddressBlock].
     *
     * @param[block]
     *   address block to generate all segments for.
     *
     * @return
     *   a range of all segment addresses within the address block.
     */
    IntProgression segmentRangeOf(AddressBlock block) {
        final int start = block.specification.prefix;
        final int subnetMask = (1 << (32 - block.specification.subnet)) - 1;
        final int end = start + subnetMask;
        return new IntProgression(start, end, 256);
    }


    UUID resourceToUuid(ResourceName name) {
        return UUID.fromString(name.getValue());
    }
}

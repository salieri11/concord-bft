/* **************************************************************************
 * Copyright (c) 2019 VMware, Inc.  All rights reserved. VMware Confidential
 * *************************************************************************/
package com.vmware.blockchain.ipam.services

import com.vmware.blockchain.deployment.persistence.kv.InMemoryUntypedKeyValueStore
import com.vmware.blockchain.deployment.persistence.kv.KeyValueStore
import com.vmware.blockchain.deployment.persistence.kv.MonotonicInt
import com.vmware.blockchain.deployment.persistence.kv.TypedKeyValueStore
import com.vmware.blockchain.deployment.service.ipam.ResourceName
import com.vmware.blockchain.deployment.v1.Address
import com.vmware.blockchain.deployment.v1.AddressBlock
import com.vmware.blockchain.deployment.v1.AddressBlockSegment
import com.vmware.blockchain.deployment.v1.AddressBlockSpecification
import com.vmware.blockchain.deployment.v1.AllocateAddressRequest
import com.vmware.blockchain.deployment.v1.AllocateAddressResponse
import com.vmware.blockchain.deployment.v1.CreateAddressBlockRequest
import com.vmware.blockchain.deployment.v1.DeleteAddressBlockRequest
import com.vmware.blockchain.deployment.v1.MessageHeader
import com.vmware.blockchain.deployment.v1.ReleaseAddressRequest
import com.vmware.blockchain.deployment.v1.ReleaseAddressResponse
import com.vmware.blockchain.ipam.server.IPAllocationService
import com.vmware.blockchain.protobuf.kotlinx.serialization.ByteString
import kotlinx.coroutines.Dispatchers
import kotlinx.coroutines.reactive.awaitSingle
import kotlinx.coroutines.runBlocking
import org.assertj.core.api.Assertions
import org.junit.jupiter.api.Test
import org.junit.jupiter.params.ParameterizedTest
import org.junit.jupiter.params.provider.ValueSource

/** Alias of [AddressBlock] key-value store for test code brevity. */
typealias BlockStore = KeyValueStore<ResourceName, AddressBlock, MonotonicInt>

/** Alias of [AddressBlockSegment] key-value store for test code brevity. */
typealias SegmentStore = KeyValueStore<ResourceName, AddressBlockSegment, MonotonicInt>

/**
 * Basic functionality test of [IPAllocationService].
 */
class IPAllocationServiceTest {

    /**
     * Create a new instance of [IPAllocationService] and return a [Triple] of the instance, the
     * backing address block and block segment [KeyValueStore]s.
     *
     * @return
     *   a triple of the service instance and the backing persistence stores.
     */
    private fun newIPAllocationService(): Triple<IPAllocationService, BlockStore, SegmentStore> {
        val blockStore = TypedKeyValueStore(
                ResourceName.serializer(),
                AddressBlock.serializer(),
                InMemoryUntypedKeyValueStore(versionSerializer = MonotonicInt.serializer())
        )
        val segmentStore = TypedKeyValueStore(
                ResourceName.serializer(),
                AddressBlockSegment.serializer(),
                InMemoryUntypedKeyValueStore(versionSerializer = MonotonicInt.serializer())
        )
        val allocator = IPAllocationService(Dispatchers.Default, blockStore, segmentStore)

        return Triple(allocator, blockStore, segmentStore)
    }

    /**
     * Test basic creation and deletion of [AddressBlock]s.
     */
    @Test
    fun testAddressBlockCreationDeletion() {
        val (allocator, blockStore, segmentStore) = newIPAllocationService()
        val blockId = "testAddressBlockCreationDeletion"
        val prefix = 0x0A010000
        val subnet = 23

        runBlocking {
            (1..100).forEach {
                val messageId = "$blockId-$it"
                val createResponse = allocator.createAddressBlock(
                        CreateAddressBlockRequest(
                                header = MessageHeader(id = messageId),
                                blockId = blockId,
                                block = AddressBlockSpecification(prefix, subnet),
                                reservedAllocations = mapOf(
                                        0x0A010000 to ByteString.of(*(ByteArray(32)
                                                .apply { set(0, 0x1) }))
                                )
                        )
                )
                Assertions.assertThat(createResponse.name).endsWith(blockId)

                allocator.deleteAddressBlock(
                        DeleteAddressBlockRequest(
                                header = MessageHeader(id = messageId),
                                name = createResponse.name
                        )
                )

                val versioned = blockStore[ResourceName(createResponse.name)].awaitSingle()
                Assertions.assertThat(versioned).isEqualTo(KeyValueStore.Versioned.None)
            }
        }

        // Cleanup
        blockStore.close()
        segmentStore.close()
    }

    /**
     * Test basic allocate and release operations on a given [AddressBlock] (of varying subnet
     * sizes).
     */
    @ParameterizedTest
    @ValueSource(ints = [23, 24, 28, 30])
    fun testAddressAllocationRelease(subnet: Int) {
        val (allocator, blockStore, segmentStore) = newIPAllocationService()
        val blockId = "testAddressAllocation"
        val prefix = 0x0A010000
        val subnetMask = (1 shl (32 - subnet)) - 1
        val blockEnd = prefix + subnetMask
        val lastSegment = blockEnd and ((1 shl 8) - 1).inv()
        val resources = mutableListOf<Address>()

        runBlocking {
            // Reserve 0th address (subnet), 1st address (gateway) and last address (broadcast).
            val reservations = when (lastSegment) {
                prefix -> mapOf(
                        prefix to ByteString.of(*ByteArray(32).apply { set(0, 0x3); set(31, -128) })
                )
                else -> mapOf(
                        prefix to ByteString.of(*ByteArray(32).apply { set(0, 0x3) }),
                        lastSegment to ByteString.of(*ByteArray(32).apply { set(31, -128) })
                )
            }
            val createResponse = allocator.createAddressBlock(
                    CreateAddressBlockRequest(
                            header = MessageHeader(id = "$blockId-create"),
                            blockId = blockId,
                            block = AddressBlockSpecification(prefix, subnet),
                            reservedAllocations = reservations
                    )
            )
            Assertions.assertThat(createResponse.name).endsWith(blockId)

            // Allocate up to full capacity, accounting for pre-allocation.
            val allocated = mutableListOf(
                    prefix,
                    prefix + 1,
                    prefix + subnetMask
            )
            (0..subnetMask - 3).forEach {
                val messageId = "$blockId-allocate-$it"
                val allocateResponse = allocator.allocateAddress(
                        AllocateAddressRequest(
                                header = MessageHeader(id = messageId),
                                parent = createResponse.name
                        )
                )

                Assertions.assertThat(allocateResponse.status)
                        .isEqualTo(AllocateAddressResponse.Status.OK)
                Assertions.assertThat(allocateResponse.address.value).isBetween(prefix, blockEnd)
                Assertions.assertThat(allocateResponse.address.value).isNotIn(allocated)

                allocated += allocateResponse.address.value
                resources += allocateResponse.address
            }

            // Additional allocation should now fail.
            val failAllocateResponse = allocator.allocateAddress(
                    AllocateAddressRequest(
                            header = MessageHeader(id = "$blockId-fail-allocation-full"),
                            parent = createResponse.name
                    )
            )
            Assertions.assertThat(failAllocateResponse.status)
                    .isEqualTo(AllocateAddressResponse.Status.RESOURCE_FULL)

            // Release all resources.
            for (resource in resources) {
                val releaseResponse = allocator.releaseAddress(
                        ReleaseAddressRequest(
                                header = MessageHeader(id = "$resource-release"),
                                name = resource.name
                        )
                )
                Assertions.assertThat(releaseResponse.status)
                        .isEqualTo(ReleaseAddressResponse.Status.OK)
            }

            // Allocate up to full capacity again, accounting for pre-allocation.
            val reallocated = mutableListOf(
                    prefix,
                    prefix + 1,
                    prefix + subnetMask
            ) // Pre-allocation.
            (0..subnetMask - 3).forEach {
                val messageId = "$blockId-reallocate-$it"
                val allocateResponse = allocator.allocateAddress(
                        AllocateAddressRequest(
                                header = MessageHeader(id = messageId),
                                parent = createResponse.name
                        )
                )

                Assertions.assertThat(allocateResponse.status)
                        .isEqualTo(AllocateAddressResponse.Status.OK)
                Assertions.assertThat(allocateResponse.address.value).isBetween(prefix, blockEnd)
                Assertions.assertThat(allocateResponse.address.value).isNotIn(reallocated)
            }

            // Additional allocation should now fail again.
            val failAgainAllocateResponse = allocator.allocateAddress(
                    AllocateAddressRequest(
                            header = MessageHeader(id = "$blockId-fail-allocation-full-again"),
                            parent = createResponse.name
                    )
            )
            Assertions.assertThat(failAgainAllocateResponse.status)
                    .isEqualTo(AllocateAddressResponse.Status.RESOURCE_FULL)

            // Remove the entire block.
            allocator.deleteAddressBlock(
                    DeleteAddressBlockRequest(
                            header = MessageHeader(id = "$blockId-delete"),
                            name = createResponse.name
                    )
            )

            // Allocate should fail.
            val notFoundAllocateResponse = allocator.allocateAddress(
                    AllocateAddressRequest(
                            header = MessageHeader(id = "$blockId-fail-allocation-not-found"),
                            parent = createResponse.name
                    )
            )
            Assertions.assertThat(notFoundAllocateResponse.status)
                    .isEqualTo(AllocateAddressResponse.Status.RESOURCE_NOT_FOUND)
        }

        // Cleanup
        blockStore.close()
        segmentStore.close()
    }
}

/* **************************************************************************
 * Copyright (c) 2019 VMware, Inc.  All rights reserved. VMware Confidential
 * *************************************************************************/
package com.vmware.blockchain.deployment.service.ipam

import com.vmware.blockchain.deployment.logging.error
import com.vmware.blockchain.deployment.logging.info
import com.vmware.blockchain.deployment.logging.logger
import com.vmware.blockchain.deployment.model.Address
import com.vmware.blockchain.deployment.model.AddressBlock
import com.vmware.blockchain.deployment.model.AddressBlockSegment
import com.vmware.blockchain.deployment.model.AllocateAddressRequest
import com.vmware.blockchain.deployment.model.AllocateAddressResponse
import com.vmware.blockchain.deployment.model.CreateAddressBlockRequest
import com.vmware.blockchain.deployment.model.CreateAddressBlockResponse
import com.vmware.blockchain.deployment.model.DeleteAddressBlockRequest
import com.vmware.blockchain.deployment.model.DeleteAddressBlockResponse
import com.vmware.blockchain.deployment.model.IPAllocationServiceImplBase
import com.vmware.blockchain.deployment.model.MessageHeader
import com.vmware.blockchain.deployment.model.ReleaseAddressRequest
import com.vmware.blockchain.deployment.model.ReleaseAddressResponse
import com.vmware.blockchain.deployment.persistence.kv.KeyValueStore
import com.vmware.blockchain.deployment.persistence.kv.KeyValueStore.Versioned
import com.vmware.blockchain.deployment.persistence.kv.MonotonicInt
import com.vmware.blockchain.deployment.persistence.kv.VersionMismatchException
import com.vmware.blockchain.protobuf.kotlinx.serialization.ByteString
import io.grpc.stub.StreamObserver
import kotlinx.coroutines.CoroutineScope
import kotlinx.coroutines.Dispatchers
import kotlinx.coroutines.Job
import kotlinx.coroutines.SupervisorJob
import kotlinx.coroutines.cancelAndJoin
import kotlinx.coroutines.joinAll
import kotlinx.coroutines.launch
import kotlinx.coroutines.reactive.awaitSingle
import kotlin.coroutines.CoroutineContext
import kotlin.math.min

/**
 * An implementation of IP allocation service.
 *
 * Note:
 *
 * 1. This implementation is not algorithmically robust around address block deletion due to the
 *    lack of transactional semantics guarding updates against on address block store and address
 *    block segment store.
 *
 *    Specifically, the follow scenarios is possible:
 *    - Concurrent block deletion and address (de)allocation leading to a segment record being
 *      deleted and then resurrected due to an update in the allocation operation writing the record
 *      back in.
 *
 *    To address the inconsistency, there are several possibility:
 *    - Two-phase commit:
 *      This leads to substantially more IO per update to either the block or segment store, which
 *      is undesirable to allocation/de-allocation workflow.
 *    - Tomb-stone deletion instead of actual deletion:
 *      Insight here is that the main problem with inconsistency arises from a potential update can
 *      only utilize conflict-detection if the record exists in the first place.
 *      This leads to additional storage for every address block created. And in the pathological
 *      case where new blocks are continually created but logically short-lived, the scheme leads
 *      to unbounded storage growth.
 *    - Flatten address block and address block segment storage into 1 storage backing:
 *      The interface contracts around [KeyValueStore] interface makes it possible to implement a
 *      CQRS-based update workflow to ensure serializability around updates.
 *
 * 2. The implementation does not guard against A-B-A type of update workflow where 2 concurrent
 *    clients (A) both attempt to release the same address while another 3rd client (B) asked for
 *    allocation and happened to be assigned the freed address. In this case (B)'s allocation may be
 *    erroneously freed by the 2nd (A) release.
 *
 *    To address this type of workflow, [Address] as a resource should have additional epoch-form
 *    of identification of uniqueness. Client will need to supply this information to ensure that
 *    A-B-A updates can be detectable due to non-matching epoch.
 *
 *    This increases the cost of storing (and updating) address allocations as [AddressBlockSegment]
 *    will need to store additional epoch value for all currently allocated addresses. Given that
 *    there are only 256 addresses, a "mapping" of allocated address to an epoch value can be
 *    encoded within a single 32-bit integer while allowing 24 bits worth of epoch precision. This
 *    could be a viable approach if concurrent allocation/de-allocation proves to be problematic for
 *    concurrent client workflow.
 */
class IPAllocationService(
    private val context: CoroutineContext = Dispatchers.Default,
    private val blockStore: KeyValueStore<ResourceName, AddressBlock, MonotonicInt>,
    private val segmentStore: KeyValueStore<ResourceName, AddressBlockSegment, MonotonicInt>
) : IPAllocationServiceImplBase(), CoroutineScope {

    /** Logging instance. */
    private val log by logger()

    /** [CoroutineContext] to launch all coroutines associated with this instance. */
    override val coroutineContext: CoroutineContext
        get() = context + job

    /** Parent [Job] of all coroutines associated with this instance's operation. */
    private val job: Job = SupervisorJob()

    /** Common initial version value to use for resource creation. */
    private val initialVersion: MonotonicInt = MonotonicInt(0)

    /**
     * Extension function to retrieve a [Byte] from a [ByteString].
     *
     * @param[index]
     *   index from the beginning of the logical data content denoted by this instance.
     *
     * @return
     *   [Byte] value at the index location.
     */
    private operator fun ByteString.get(index: Int): Byte {
        return if (index in (0 until length)) {
            data[offset + index]
        } else {
            throw IndexOutOfBoundsException("Index($index) is out of range, length($length)")
        }
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
    private fun blockName(block: String): ResourceName = ResourceName("blocks/$block")

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
    private fun segmentName(blockResource: String, segment: Int): ResourceName {
        return ResourceName("$blockResource/segments/${segment.toString(16).padStart(8, '0')}")
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
    private fun segmentName(blockResource: ResourceName, segment: Int): ResourceName {
        return segmentName(blockResource.value, segment)
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
    private fun addressName(segmentResource: String, address: Int): ResourceName {
        return ResourceName("$segmentResource/addresses/${address.toString(16).padStart(8, '0')}")
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
    private fun addressName(segmentResource: ResourceName, address: Int): ResourceName {
        return addressName(segmentResource.value, address)
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
    private fun segmentRangeOf(block: AddressBlock): IntProgression {
        val start = block.specification.prefix
        val subnetMask = (1 shl (32 - block.specification.subnet)) - 1
        val end = start + subnetMask

        return start until end step 256
    }

    /**
     * Ensure that a block specified by a given name is in DELETING state, or return Versioned.None
     * if while attempting to transition into DELETING state, the block is already gone.
     *
     * @param[name]
     *   resource name of the block.
     *
     * @return
     *   the final versioned value of the [AddressBlock] as persisted with DELETING state.
     */
    private suspend fun ensureDeletingBlock(
        name: ResourceName
    ): Versioned<AddressBlock, MonotonicInt> {
        return when (val initial = blockStore[name].awaitSingle()) {
            is Versioned.Just -> {
                var nextVersioned: Versioned<AddressBlock, MonotonicInt> = initial
                var (finalBlock, finalBlockVersion) = initial
                while (finalBlock.state != AddressBlock.State.DELETING) {
                    // Set next attempt.
                    finalBlock = finalBlock.copy(state = AddressBlock.State.DELETING)

                    try {
                        blockStore.set(name, finalBlockVersion, finalBlock).awaitSingle()

                        // Store command did not fail, so block should be in deleting state.
                        nextVersioned = Versioned.Just(finalBlock, finalBlockVersion.next())
                    } catch (error: VersionMismatchException) {
                        nextVersioned = blockStore[name].awaitSingle()
                        when (nextVersioned) {
                            is Versioned.Just -> {
                                finalBlock = nextVersioned.value
                                finalBlockVersion = nextVersioned.version
                            }
                            is Versioned.None -> {
                                // Record is no longer there.
                                nextVersioned = Versioned.None
                            }
                        }
                    }
                }

                // Return true as result (record is not there anymore or is in deleting state.
                nextVersioned
            }
            is Versioned.None -> initial
        }
    }

    /**
     * Ensure that a block segment specified by a given name and a given epoch is deleted from
     * backing store.
     *
     * @param[name]
     *   resource name of the block segment.
     * @param[epoch]
     *   uniqueness number guarding/identifying the block segment to be deleted
     */
    private suspend fun ensureDeletedSegment(name: ResourceName, epoch: Int) {
        var versioned = segmentStore[name].awaitSingle()
        while (versioned !is Versioned.None) {
            val (segment, version) = versioned as Versioned.Just<AddressBlockSegment, MonotonicInt>
            if (segment.epoch != epoch) {
                // The segment that corresponded to the epoch parameter is already gone.
                break
            } else {
                versioned = try {
                    segmentStore.delete(name, version).awaitSingle()
                } catch (error: VersionMismatchException) {
                    segmentStore[name].awaitSingle()
                }
            }
        }
    }

    /**
     * Allocate an [Address] from one of [AddressBlock]'s address segment.
     *
     * @param[block]
     *   address block to allocate address from.
     * @param[segmentAddress]
     *   block segment within the address block to allocate address from.
     *
     * @return
     *   an [Address] if allocation was successful, `null` otherwise.
     */
    private suspend fun allocateFromBlock(block: AddressBlock, segmentAddress: Int): Address? {
        var allocation: Address? = null
        val name = segmentName(block.name, segmentAddress)

        // Attempt to allocate from this segment as long as:
        // 1. Segment still exists.
        // 2. Segment has space and the write-attempt to ink-in the allocation succeeds.
        var isSegmentFull = false
        while (allocation == null && !isSegmentFull) {
            allocation = when (val versioned = segmentStore[name].awaitSingle()) {
                is Versioned.None -> {
                    // Brand new segment, use the first address.
                    val newSegment = AddressBlockSegment(
                            name = name.value,
                            epoch = block.epoch,
                            segment = segmentAddress,
                            allocations = ByteString.of(*(ByteArray(32).apply { set(0, 1) }))
                    )
                    try {
                        segmentStore.set(name, initialVersion, newSegment).awaitSingle()
                        Address(addressName(name, segmentAddress).value, segmentAddress)
                    } catch (error: VersionMismatchException) {
                        // Version mismatch, re-fetch the record and try again.
                        null
                    }
                }
                is Versioned.Just -> {
                    // Only proceed if latest value corresponded to the epoch parameter.
                    if (versioned.value.epoch != block.epoch) {
                        isSegmentFull = true
                        null
                    } else {
                        val (segment, version) = versioned
                        val subnetMask = (1 shl (32 - block.specification.subnet)) - 1
                        val blockEnd = block.specification.prefix + subnetMask - 1
                        val limit = min(blockEnd, segment.segment + 255)

                        val allocated = allocateFromSegment(segment, limit)
                        if (allocated != null) {
                            val (newSegment, address) = allocated
                            try {
                                segmentStore.set(name, version, newSegment).awaitSingle()
                                Address(addressName(name, address).value, address)
                            } catch (error: VersionMismatchException) {
                                /* Version mismatch, re-fetch the record and try again.*/
                                null
                            }
                        } else {
                            isSegmentFull = true
                            null
                        }
                    }
                }
            }
        }

        return allocation
    }

    /**
     * Allocate an [Address] from one of [AddressBlockSegment].
     *
     * @param[segment]
     *   address block segment to allocate address from.
     * @param[limit]
     *   search limit within the segment (to prevent allocation beyond a address block range for
     *   blocks whose range is smaller than a /24 segment).
     *
     * @return
     *   a [Pair] of [AddressBlockSegment] to [Int] indicating the final value state of the
     *   address block segment after a successful allocation, and the integer value corresponding to
     *   the address that was marked as allocated, `null` otherwise.
     */
    private fun allocateFromSegment(
        segment: AddressBlockSegment,
        limit: Int
    ): Pair<AddressBlockSegment, Int>? {
        // Look for a free bit.
        for (index in 0..31) {
            val byte = segment.allocations[index]
            for (bit in 0..7) {
                if (byte.toInt() and (1 shl bit) == 0) {
                    // Found a free bit.
                    val address = segment.segment + (8 * index + bit)

                    // Make sure we are still in allocation range with respect to block range.
                    if (address !in segment.segment..limit) {
                        return null
                    }

                    val newByte = (byte.toInt() or (1 shl bit)).toByte()
                    val newAllocations = segment.allocations.toByteArray()
                            .apply { set(index, newByte) }

                    return segment.copy(allocations = ByteString.of(*newAllocations)) to address
                }
            }
        }

        return null
    }

    private fun releaseFromSegment(
        segment: AddressBlockSegment,
        address: Int
    ): AddressBlockSegment {
        require(address in segment.segment..(segment.segment + 255))

        val offset = address - segment.segment
        val byte = segment.allocations[offset ushr 3]
        val bitMask = 1 shl (offset.rem(8))

        return if (byte.toInt() and bitMask == bitMask) {
            val newByte = (byte.toInt() and (1 shl (offset.rem(8))).inv()).toByte()
            val newAllocations = segment.allocations.toByteArray()
                    .apply { set(offset ushr 3, newByte) }
            segment.copy(allocations = ByteString.of(*newAllocations))
        } else {
            // There is no bit to clear, so just return the input value.
            // Note: It is possible that the caller's value is stale, which caller will find out
            // when it attempts to write this segment back into the data store.
            segment
        }
    }

    /**
     * Shutdown the service instance.
     */
    suspend fun shutdown() {
        job.cancelAndJoin()
    }

    override fun createAddressBlock(
        request: CreateAddressBlockRequest,
        responseObserver: StreamObserver<CreateAddressBlockResponse>
    ) {
        launch(coroutineContext) {
            // Precondition checks on input.
            val model = requireNotNull(request.block)
            val blockName = blockName(requireNotNull(request.blockId))
            val reservations = requireNotNull(request.reservedAllocations)
            val subnetMask = (1 shl (32 - model.subnet)) - 1

            // Block should start on a subnet's 0th address.
            check((model.prefix and subnetMask) == 0)

            for ((segment, allocation) in reservations) {
                // Pre-allocations should be within the block range.
                check(segment in model.prefix..(model.prefix + subnetMask))

                // Allocation bitmap should contain 256 bits exactly.
                check(allocation.length == 32)
            }

            // Create the initial block (set to CREATING state to gate concurrent attempts).
            // TODO: change to a meaningful logical time value.
            val epoch = 0
            val initialBlock = AddressBlock(
                    name = blockName.value,
                    specification = model,
                    epoch = epoch,
                    state = AddressBlock.State.CREATING
            )
            when (blockStore.set(blockName, initialVersion, initialBlock).awaitSingle()) {
                is Versioned.Just -> {
                    throw IllegalStateException("Block($blockName) already exists")
                }
                is Versioned.None -> { // Successful initial creation.
                    // Allocation with pre-allocated blocks.
                    for ((segmentAddress, allocated) in reservations) {
                        val segmentName = segmentName(blockName, segmentAddress)
                        val segment = AddressBlockSegment(
                                name = segmentName.value,
                                epoch = epoch,
                                segment = segmentAddress,
                                allocations = allocated
                        )

                        // Store the segments (one at a time because parallel doesn't have any
                        // benefit against a single datastore source).
                        segmentStore.set(segmentName, initialVersion, segment)
                                // Note: awaitSingle() may cause exception to be thrown, which is
                                // then handled by the general completion handler and onError()
                                // signaled accordingly to observer's client.
                                .awaitSingle()
                    }

                    // Write the block again signaling the block is active for allocation requests.
                    val block = initialBlock.copy(state = AddressBlock.State.ACTIVE)
                    when (blockStore.set(blockName, initialVersion.next(), block).awaitSingle()) {
                        is Versioned.None -> {
                            throw IllegalStateException("Block($blockName) removed during creation")
                        }
                        is Versioned.Just -> {
                            // Send the response signal to indicate successful creation.
                            responseObserver.onNext(
                                    CreateAddressBlockResponse(
                                            header = MessageHeader(id = request.header.id),
                                            name = blockName.value
                                    )
                            )
                        }
                    }
                }
            }
        }.invokeOnCompletion { error ->
            if (error != null) {
                log.error { "Error creating block, id(${request.blockId}), error(${error.message})" }

                responseObserver.onError(error)
            } else {
                log.info { "Created address block(${blockName(request.blockId)})" }

                responseObserver.onCompleted()
            }
        }
    }

    override fun deleteAddressBlock(
        request: DeleteAddressBlockRequest,
        responseObserver: StreamObserver<DeleteAddressBlockResponse>
    ) {
        launch(coroutineContext) {
            // Precondition checks on input.
            val name = ResourceName(requireNotNull(request.name))

            when (val deletingValue = ensureDeletingBlock(name)) {
                is Versioned.Just -> {
                    // Examine the segments that were allocated during the "final" version.
                    val block = deletingValue.value
                    segmentRangeOf(block)
                            .map {
                                launch(coroutineContext) {
                                    /* Delete segments.*/
                                    ensureDeletedSegment(segmentName(block.name, it), block.epoch)
                                }
                            }
                            .joinAll()

                    // Assuming no failures after waiting for all segment deletes via joinAll().
                    blockStore.delete(name, deletingValue.version).awaitSingle()
                }
            }

            // Emit a response (Should there be a success / failure indicator?).
            responseObserver.onNext(
                    DeleteAddressBlockResponse(header = MessageHeader(id = request.header.id))
            )
        }.invokeOnCompletion { error ->
            if (error != null) {
                log.error { "Error deleting block(${request.name}), error(${error.message}" }

                responseObserver.onError(error)
            } else {
                log.info { "Deleted address block(${request.name})" }

                responseObserver.onCompleted()
            }
        }
    }

    override fun allocateAddress(
        request: AllocateAddressRequest,
        responseObserver: StreamObserver<AllocateAddressResponse>
    ) {
        launch(coroutineContext) {
            // Precondition checks on input.
            val nameFragments = requireNotNull(request.parent).split("/")
            require(nameFragments.size == 2)

            var address: Address? = null
            val blockName = blockName(nameFragments[1])
            val response = when (val versioned = blockStore[blockName].awaitSingle()) {
                is Versioned.Just -> {
                    val block = versioned.value

                    // Look for space among segments.
                    // Note (improvement):
                    // Server should maintain a cache of latest released addresses per block to
                    // avoid scanning through all segments when block is fairly allocated near the
                    // beginning.
                    for (segmentAddress in segmentRangeOf(block)) {
                        address = allocateFromBlock(block, segmentAddress)
                        if (address != null) {
                            // Stop searching.
                            break
                        }
                    }

                    if (address == null) {
                        AllocateAddressResponse(
                                header = MessageHeader(id = request.header.id),
                                status = AllocateAddressResponse.Status.RESOURCE_FULL
                        )
                    } else {
                        AllocateAddressResponse(
                                header = MessageHeader(id = request.header.id),
                                status = AllocateAddressResponse.Status.OK,
                                address = address
                        )
                    }
                }
                is Versioned.None -> {
                    AllocateAddressResponse(
                            header = MessageHeader(id = request.header.id),
                            status = AllocateAddressResponse.Status.RESOURCE_NOT_FOUND
                    )
                }
            }

            // Send the response.
            if (response.status == AllocateAddressResponse.Status.OK) {
                log.info { "Allocated address(${response.address.name}) from block($blockName)" }
            } else {
                log.info { "Unable to allocate address from block($blockName)" }
            }

            responseObserver.onNext(response)
        }.invokeOnCompletion { error ->
            if (error != null) {
                log.error { "Error allocating from block(${request.parent}), error(${error.message}" }

                responseObserver.onError(error)
            } else {
                responseObserver.onCompleted()
            }
        }
    }

    override fun releaseAddress(
        request: ReleaseAddressRequest,
        responseObserver: StreamObserver<ReleaseAddressResponse>
    ) {
        var released = false

        launch(coroutineContext) {
            // Precondition checks on input.
            val nameFragments = requireNotNull(request.name).split("/")
            require(nameFragments.size == 6)

            val segmentName = segmentName(blockName(nameFragments[1]), nameFragments[3].toInt(16))
            val address = nameFragments[5].toInt(16)

            var exist = true
            while (!released && exist) {
                exist = when (val versioned = segmentStore[segmentName].awaitSingle()) {
                    is Versioned.Just -> {
                        released = releaseFromSegment(versioned.value, address)
                                .let {
                                    try {
                                        segmentStore.set(segmentName, versioned.version, it)
                                                .awaitSingle()
                                        true
                                    } catch (error: VersionMismatchException) {
                                        false
                                    }
                                }

                        // If released, exist is false, and vice-versa.
                        !released
                    }
                    is Versioned.None -> false
                }
            }
        }.invokeOnCompletion { error ->
            if (error != null) {
                log.error { "Error releasing address(${request.name}), error(${error.message}" }

                responseObserver.onError(error)
            } else {
                // If released as part of this execution, the resource was there.
                val status = when (released) {
                    false -> ReleaseAddressResponse.Status.RESOURCE_NOT_FOUND
                    true -> ReleaseAddressResponse.Status.OK
                }

                responseObserver.onNext(ReleaseAddressResponse(status = status))
                responseObserver.onCompleted()

                log.info { "Released address(${request.name}), status($status)" }
            }
        }
    }
}

/* **************************************************************************
 * Copyright (c) 2019 VMware, Inc.  All rights reserved. VMware Confidential
 * *************************************************************************/
package com.vmware.blockchain.ipam.server

import com.google.protobuf.ByteString
import com.vmware.blockchain.common.NotFoundException
import com.vmware.blockchain.dao.ConcurrentUpdateException
import com.vmware.blockchain.dao.GenericDao
import com.vmware.blockchain.deployment.logging.logger
import com.vmware.blockchain.deployment.v1.Address
import com.vmware.blockchain.deployment.v1.AllocateAddressRequest
import com.vmware.blockchain.deployment.v1.AllocateAddressResponse
import com.vmware.blockchain.deployment.v1.CreateAddressBlockRequest
import com.vmware.blockchain.deployment.v1.CreateAddressBlockResponse
import com.vmware.blockchain.deployment.v1.DeleteAddressBlockRequest
import com.vmware.blockchain.deployment.v1.DeleteAddressBlockResponse
import com.vmware.blockchain.deployment.v1.IPAllocationServiceGrpc
import com.vmware.blockchain.deployment.v1.MessageHeader
import com.vmware.blockchain.deployment.v1.ReleaseAddressRequest
import com.vmware.blockchain.deployment.v1.ReleaseAddressResponse
import io.grpc.stub.StreamObserver
import kotlinx.coroutines.CoroutineScope
import kotlinx.coroutines.Dispatchers
import kotlinx.coroutines.Job
import kotlinx.coroutines.SupervisorJob
import kotlinx.coroutines.joinAll
import kotlinx.coroutines.launch
import org.springframework.beans.factory.annotation.Autowired
import org.lognet.springboot.grpc.GRpcService
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
@GRpcService
class IPAllocationService @Autowired constructor(
        private val context: CoroutineContext = Dispatchers.Default,
        private val genericDao: GenericDao,
        private val ipAllocationUtil: IpAllocationUtil
) : IPAllocationServiceGrpc.IPAllocationServiceImplBase(), CoroutineScope {

    /** Logging instance. */
    private val log by logger()

    /** [CoroutineContext] to launch all coroutines associated with this instance. */
    override val coroutineContext: CoroutineContext
        get() = context + job

    /** Parent [Job] of all coroutines associated with this instance's operation. */
    private val job: Job = SupervisorJob()

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
    ): AddressBlock? {
        return try {
            var initial = genericDao.get(ipAllocationUtil.resourceToUuid(name), AddressBlock :: class.java)
            var finalBlock = initial
            var nextVersioned = initial

            while (finalBlock.state != AddressBlock.State.DELETING) {
                // Set next attempt.
                finalBlock.setState(AddressBlock.State.DELETING)

                try {
                    finalBlock.setId(ipAllocationUtil.resourceToUuid(name))
                    nextVersioned = genericDao.put(finalBlock, finalBlock)

                    // Store command did not fail, so block should be in deleting state.
                    // put returns new entity. Is this the same as the previous code?
                    // nextVersioned = genericDao.get(ipAllocationUtil.resourceToUuid(name), AddressBlock::class.java)
                } catch (error: ConcurrentUpdateException) {
                    try {
                        nextVersioned = genericDao.get(ipAllocationUtil.resourceToUuid(name), AddressBlock::class.java)
                        finalBlock = nextVersioned
                    } catch (e: NotFoundException) {
                        nextVersioned = null
                    }
                }
            }
            nextVersioned
        } catch (e: NotFoundException) {
            null
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
        try {
            var addressBlockSegment: AddressBlockSegment? = genericDao.get(ipAllocationUtil.resourceToUuid(name),
                    AddressBlockSegment::class.java)
            while(addressBlockSegment != null) {
                if (addressBlockSegment.getVersion() != epoch) {
                    break
                } else {
                    addressBlockSegment = try {
                        genericDao.delete(ipAllocationUtil.resourceToUuid(name), AddressBlockSegment::class.java)
                        genericDao.get(ipAllocationUtil.resourceToUuid(name), AddressBlockSegment::class.java)
                    } catch (error: Exception) {
                        genericDao.get(ipAllocationUtil.resourceToUuid(name), AddressBlockSegment::class.java)
                    }
                }
            }
        } catch(error: NotFoundException) {
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
        val name = ipAllocationUtil.segmentName(block.name, segmentAddress)

        // Attempt to allocate from this segment as long as:
        // 1. Segment still exists.
        // 2. Segment has space and the write-attempt to ink-in the allocation succeeds.
        var isSegmentFull = false
        while (allocation == null && !isSegmentFull) {
            allocation = try {
                var addressBlockSegment : AddressBlockSegment =
                        genericDao.get(ipAllocationUtil.resourceToUuid(name), AddressBlockSegment::class.java)

                if (addressBlockSegment.getVersion() != block.getVersion()) {
                    isSegmentFull = true
                    null
                } else {
                    val subnetMask = (1 shl (32 - block.getSpecification().subnet)) - 1
                    val blockEnd = block.getSpecification().prefix + subnetMask - 1
                    val limit = min(blockEnd, addressBlockSegment.getSegment() + 255)

                    val allocated = allocateFromSegment(addressBlockSegment, limit)

                    if (allocated != null) {
                        val (newSegment, address) = allocated
                        try {
                            genericDao.put(newSegment, newSegment)
                            Address.newBuilder().setName(ipAllocationUtil.addressName(name, segmentAddress).value)
                                    .setValue(segmentAddress).build()
                        } catch (error: ConcurrentUpdateException) {
                            /* Version mismatch, re-fetch the record and try again.*/
                            null
                        }
                    } else {
                        isSegmentFull = true
                        null
                    }
                }
            } catch (e: NotFoundException) {
                val newSegment = AddressBlockSegment(name.value, segmentAddress,
                        ByteString.copyFrom((ByteArray(32).apply { set(0, 1) })))

                try {
                    genericDao.put(newSegment, null)
                    Address.newBuilder().setName(ipAllocationUtil.addressName(name, segmentAddress).value)
                            .setValue(segmentAddress).build()
                } catch (error: ConcurrentUpdateException) {
                    null
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
            val byte = segment.allocations.byteAt(index)
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

                    return AddressBlockSegment(segment.name, segment.segment, ByteString.copyFrom(newAllocations)) to address
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
        val byte = segment.allocations.byteAt(offset ushr 3)
        val bitMask = 1 shl (offset.rem(8))

        return if (byte.toInt() and bitMask == bitMask) {
            val newByte = (byte.toInt() and (1 shl (offset.rem(8))).inv()).toByte()
            val newAllocations = segment.allocations.toByteArray()
                    .apply { set(offset ushr 3, newByte) }
            AddressBlockSegment(segment.name, segment.segment, ByteString.copyFrom(newAllocations))
            // AddressBlockSegment.newBuilder(segment).setAllocations(ByteString.copyFrom(newAllocations)).build()
        } else {
            // There is no bit to clear, so just return the input value.
            // Note: It is possible that the caller's value is stale, which caller will find out
            // when it attempts to write this segment back into the data store.
            segment
        }
    }

    override fun createAddressBlock(
            request: CreateAddressBlockRequest,
            responseObserver: StreamObserver<CreateAddressBlockResponse>
    ) {
        launch(coroutineContext) {
            // Precondition checks on input.
            val model = requireNotNull(request.block)
            val blockName = ipAllocationUtil.blockName(requireNotNull(request.blockId))
            val reservations = requireNotNull(request.reservedAllocations)
            val subnetMask = (1 shl (32 - model.subnet)) - 1

            // Block should start on a subnet's 0th address.
            check((model.prefix and subnetMask) == 0)

            for ((segment, allocation) in reservations) {
                // Pre-allocations should be within the block range.
                check(segment in model.prefix..(model.prefix + subnetMask))

                // Allocation bitmap should contain 256 bits exactly.
                check(allocation.size() == 32)
            }

            val initialBlock =  AddressBlock(blockName.value, ipAllocationUtil.convertToBlockSpecification(model),
                    AddressBlock.State.CREATING)
            initialBlock.id = ipAllocationUtil.resourceToUuid(blockName)

            var existingBlock: AddressBlock? = try {
                genericDao.get(ipAllocationUtil.resourceToUuid(blockName), AddressBlock::class.java)
            } catch (e: NotFoundException) {
                null
            }

            if (existingBlock == null) {
                // No block exists
                genericDao.put(initialBlock, null)
                for ((segmentAddress, allocated) in reservations) {
                    val segmentName = ipAllocationUtil.segmentName(blockName, segmentAddress)
                    val segment = AddressBlockSegment(segmentName.value, segmentAddress, allocated)
                    segment.id = ipAllocationUtil.resourceToUuid(segmentName)
                    genericDao.put(segment, null)
                }

                // Write the block again signaling the block is active for allocation requests.
                val block = AddressBlock(initialBlock.name, initialBlock.specification,
                        AddressBlock.State.ACTIVE)
                block.id = ipAllocationUtil.resourceToUuid(blockName)
                try {
                    genericDao.put(block, block)
                    responseObserver.onNext(
                            CreateAddressBlockResponse.newBuilder()
                                    .setHeader(MessageHeader.newBuilder().setId(request.header.id))
                                    .setName(blockName.value)
                                    .build()
                    )
                } catch (e: NotFoundException) {
                    throw IllegalStateException("Block($blockName) removed during creation")
                }
            } else {
                // Block already exists. Throw an exception.
                throw IllegalStateException("Block($blockName) already exists")
            }
        }.invokeOnCompletion { error ->
            if (error != null) {
                log.error("Error creating block, id(${request.blockId}), error(${error.message})")

                responseObserver.onError(error)
            } else {
                log.info("Created address block(${ipAllocationUtil.blockName(request.blockId)})")

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

            try {
                val block = ensureDeletingBlock(name)
                if (block != null) {
                    ipAllocationUtil.segmentRangeOf(block)
                            .map {
                                launch(coroutineContext) {
                                    /* Delete segments.*/
                                    ensureDeletedSegment(ipAllocationUtil.segmentName(block.name, it), block.version)
                                }
                            }
                            .joinAll()

                    genericDao.delete(ipAllocationUtil.resourceToUuid(name), AddressBlock::class.java)
                }
            } catch (e: Error) {}

            // Emit a response (Should there be a success / failure indicator?).
            responseObserver.onNext(
                    DeleteAddressBlockResponse.newBuilder()
                            .setHeader(MessageHeader.newBuilder().setId(request.header.id)).build()
            )
        }.invokeOnCompletion { error ->
            if (error != null) {
                log.error("Error deleting block(${request.name}), error(${error.message}")

                responseObserver.onError(error)
            } else {
                log.info("Deleted address block(${request.name})")

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
            val blockName = ipAllocationUtil.blockName(nameFragments[1])
            val response: AllocateAddressResponse = try {
                val versioned = genericDao.get(ipAllocationUtil.resourceToUuid(blockName), AddressBlock::class.java)
                val block = versioned

                // Look for space among segments.
                // Note (improvement):
                // Server should maintain a cache of latest released addresses per block to
                // avoid scanning through all segments when block is fairly allocated near the
                // beginning.
                for (segmentAddress in ipAllocationUtil.segmentRangeOf(block)) {
                    address = allocateFromBlock(block, segmentAddress)
                    if (address != null) {
                        // Stop searching.
                        break
                    }
                }

                if (address == null) {
                    AllocateAddressResponse.newBuilder()
                            .setHeader(MessageHeader.newBuilder().setId(request.header.id).build())
                            .setStatus(AllocateAddressResponse.Status.RESOURCE_FULL)
                            .build()
                } else {
                    AllocateAddressResponse.newBuilder()
                            .setHeader(MessageHeader.newBuilder().setId(request.header.id).build())
                            .setStatus(AllocateAddressResponse.Status.OK)
                            .setAddress(address)
                            .build()
                }
            } catch (e: NotFoundException) {
                AllocateAddressResponse.newBuilder()
                        .setHeader(MessageHeader.newBuilder().setId(request.header.id).build())
                        .setStatus(AllocateAddressResponse.Status.RESOURCE_NOT_FOUND)
                        .build()
            }

            // Send the response.
            if (response.status == AllocateAddressResponse.Status.OK) {
                log.info("Allocated address(${response.address.name}) from block($blockName)")
            } else {
                log.info("Unable to allocate address from block($blockName)")
            }

            responseObserver.onNext(response)
        }.invokeOnCompletion { error ->
            if (error != null) {
                log.error("Error allocating from block(${request.parent}), error(${error.message}")

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

            val segmentName = ipAllocationUtil.segmentName(ipAllocationUtil.blockName(nameFragments[1]),
                    nameFragments[3].toInt(16))
            val address = nameFragments[5].toInt(16)

            var exist = true
            while (!released && exist) {
                exist = try {
                    val versioned = genericDao.get(ipAllocationUtil.resourceToUuid(segmentName),
                            AddressBlockSegment::class.java)
                    released = releaseFromSegment(versioned, address)
                            .let {
                                try {
                                    it.setId(ipAllocationUtil.resourceToUuid(segmentName))
                                    genericDao.put(it, it)
                                    true
                                } catch (error: ConcurrentUpdateException) {
                                    false
                                }
                            }
                    !released
                } catch (e: NotFoundException) {
                    false
                }
            }
        }.invokeOnCompletion { error ->
            if (error != null) {
                log.error("Error releasing address(${request.name}), error(${error.message}")

                responseObserver.onError(error)
            } else {
                // If released as part of this execution, the resource was there.
                val status = when (released) {
                    false -> ReleaseAddressResponse.Status.RESOURCE_NOT_FOUND
                    true -> ReleaseAddressResponse.Status.OK
                }

                responseObserver.onNext(ReleaseAddressResponse.newBuilder().setStatus(status).build())
                responseObserver.onCompleted()

                log.info("Released address(${request.name}), status($status)")
            }
        }
    }
}

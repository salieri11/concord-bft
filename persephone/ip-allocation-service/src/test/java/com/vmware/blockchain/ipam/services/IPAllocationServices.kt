/* **************************************************************************
 * Copyright (c) 2019 VMware, Inc.  All rights reserved. VMware Confidential
 * *************************************************************************/
package com.vmware.blockchain.ipam.services

import com.vmware.blockchain.deployment.v1.AllocateAddressRequest
import com.vmware.blockchain.deployment.v1.AllocateAddressResponse
import com.vmware.blockchain.deployment.v1.CreateAddressBlockRequest
import com.vmware.blockchain.deployment.v1.CreateAddressBlockResponse
import com.vmware.blockchain.deployment.v1.DeleteAddressBlockRequest
import com.vmware.blockchain.deployment.v1.DeleteAddressBlockResponse
import com.vmware.blockchain.deployment.v1.ReleaseAddressRequest
import com.vmware.blockchain.deployment.v1.ReleaseAddressResponse

/**
 * Suspendable form of [IPAllocationServiceOld.createAddressBlock].
 *
 * @param[request]
 *   request to the operation.
 *
 * @return
 *   message response.
 */
suspend fun IPAllocationServiceOld.createAddressBlock(
    request: CreateAddressBlockRequest
): CreateAddressBlockResponse {
    val observer = CollectingStreamObserver<CreateAddressBlockResponse>()
    createAddressBlock(request, observer)

    return observer.awaitSingle()
}

/**
 * Suspendable form of [IPAllocationServiceOld.deleteAddressBlock].
 *
 * @param[request]
 *   request to the operation.
 *
 * @return
 *   message response.
 */
suspend fun IPAllocationServiceOld.deleteAddressBlock(
    request: DeleteAddressBlockRequest
): DeleteAddressBlockResponse {
    val observer = CollectingStreamObserver<DeleteAddressBlockResponse>()
    deleteAddressBlock(request, observer)

    return observer.awaitSingle()
}

/**
 * Suspendable form of [IPAllocationServiceOld.allocateAddress].
 *
 * @param[request]
 *   request to the operation.
 *
 * @return
 *   message response.
 */
suspend fun IPAllocationServiceOld.allocateAddress(
    request: AllocateAddressRequest
): AllocateAddressResponse {
    val observer = CollectingStreamObserver<AllocateAddressResponse>()
    allocateAddress(request, observer)

    return observer.awaitSingle()
}

/**
 * Suspendable form of [IPAllocationServiceOld.releaseAddress].
 *
 * @param[request]
 *   request to the operation.
 *
 * @return
 *   message response.
 */
suspend fun IPAllocationServiceOld.releaseAddress(
    request: ReleaseAddressRequest
): ReleaseAddressResponse {
    val observer = CollectingStreamObserver<ReleaseAddressResponse>()
    releaseAddress(request, observer)

    return observer.awaitSingle()
}

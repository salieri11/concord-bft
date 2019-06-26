/* **************************************************************************
 * Copyright (c) 2019 VMware, Inc.  All rights reserved. VMware Confidential
 * *************************************************************************/
package com.vmware.blockchain.deployment.service.ipam

import com.vmware.blockchain.deployment.model.AllocateAddressRequest
import com.vmware.blockchain.deployment.model.AllocateAddressResponse
import com.vmware.blockchain.deployment.model.CreateAddressBlockRequest
import com.vmware.blockchain.deployment.model.CreateAddressBlockResponse
import com.vmware.blockchain.deployment.model.DeleteAddressBlockRequest
import com.vmware.blockchain.deployment.model.DeleteAddressBlockResponse
import com.vmware.blockchain.deployment.model.ReleaseAddressRequest
import com.vmware.blockchain.deployment.model.ReleaseAddressResponse

/**
 * Suspendable form of [IPAllocationService.createAddressBlock].
 *
 * @param[request]
 *   request to the operation.
 *
 * @return
 *   message response.
 */
suspend fun IPAllocationService.createAddressBlock(
    request: CreateAddressBlockRequest
): CreateAddressBlockResponse {
    val observer = CollectingStreamObserver<CreateAddressBlockResponse>()
    createAddressBlock(request, observer)

    return observer.awaitSingle()
}

/**
 * Suspendable form of [IPAllocationService.deleteAddressBlock].
 *
 * @param[request]
 *   request to the operation.
 *
 * @return
 *   message response.
 */
suspend fun IPAllocationService.deleteAddressBlock(
    request: DeleteAddressBlockRequest
): DeleteAddressBlockResponse {
    val observer = CollectingStreamObserver<DeleteAddressBlockResponse>()
    deleteAddressBlock(request, observer)

    return observer.awaitSingle()
}

/**
 * Suspendable form of [IPAllocationService.allocateAddress].
 *
 * @param[request]
 *   request to the operation.
 *
 * @return
 *   message response.
 */
suspend fun IPAllocationService.allocateAddress(
    request: AllocateAddressRequest
): AllocateAddressResponse {
    val observer = CollectingStreamObserver<AllocateAddressResponse>()
    allocateAddress(request, observer)

    return observer.awaitSingle()
}

/**
 * Suspendable form of [IPAllocationService.releaseAddress].
 *
 * @param[request]
 *   request to the operation.
 *
 * @return
 *   message response.
 */
suspend fun IPAllocationService.releaseAddress(
    request: ReleaseAddressRequest
): ReleaseAddressResponse {
    val observer = CollectingStreamObserver<ReleaseAddressResponse>()
    releaseAddress(request, observer)

    return observer.awaitSingle()
}

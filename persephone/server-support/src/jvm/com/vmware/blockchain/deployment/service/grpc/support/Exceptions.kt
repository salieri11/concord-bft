/* **************************************************************************
 * Copyright (c) 2019 VMware, Inc. All rights reserved. VMware Confidential
 * **************************************************************************/
package com.vmware.blockchain.deployment.service.grpc.support

import io.grpc.Status
import kotlinx.coroutines.TimeoutCancellationException

/**
 * Extension function to [Throwable] to convert to a [Status] instance.
 *
 * @return
 *   a well-known [Status] value if a matching one is found, or [Status.INTERNAL] by default.
 */
fun Throwable.toStatus(): Status {
    return when (this) {
        is IllegalArgumentException -> Status.INVALID_ARGUMENT.withCause(this)
        is IllegalStateException -> Status.FAILED_PRECONDITION.withCause(this)
        is UnsupportedOperationException -> Status.UNIMPLEMENTED.withCause(this)
        is TimeoutCancellationException -> Status.DEADLINE_EXCEEDED.withCause(this)
        else -> Status.INTERNAL.withCause(this)
    }
}

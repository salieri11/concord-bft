/* **************************************************************************
 * Copyright (c) 2019 VMware, Inc.  All rights reserved. VMware Confidential
 * *************************************************************************/
package com.vmware.blockchain.deployment.service.fleet

import com.vmware.blockchain.deployment.logging.error
import com.vmware.blockchain.deployment.logging.info
import com.vmware.blockchain.deployment.logging.logger
import com.vmware.blockchain.deployment.v1.FleetManagementServiceImplBase
import com.vmware.blockchain.deployment.v1.UpdateInstanceRequest
import com.vmware.blockchain.deployment.v1.UpdateInstanceResponse
import io.grpc.stub.StreamObserver
import kotlin.coroutines.CoroutineContext
import kotlinx.coroutines.CoroutineScope
import kotlinx.coroutines.Dispatchers
import kotlinx.coroutines.Job
import kotlinx.coroutines.SupervisorJob
import kotlinx.coroutines.cancelAndJoin
import kotlinx.coroutines.launch

/**
 * An implementation of [com.vmware.blockchain.deployment.v1.FleetManagementService].
 */
class FleetManagementService(
    private val context: CoroutineContext = Dispatchers.Default
) : FleetManagementServiceImplBase(), CoroutineScope {

    /** Logging instance. */
    private val log by logger()

    /** [CoroutineContext] to launch all coroutines associated with this instance. */
    override val coroutineContext: CoroutineContext
        get() = context + job

    /** Parent [Job] of all coroutines associated with this instance's operation. */
    private val job: Job = SupervisorJob()

    /**
     * Shutdown the service instance.
     */
    suspend fun shutdown() {
        job.cancelAndJoin()
    }

    override fun updateInstance(
        request: UpdateInstanceRequest,
        responseObserver: StreamObserver<UpdateInstanceResponse>
    ) {
        launch(coroutineContext) {
            // Precondition checks on input.
        }.invokeOnCompletion { error ->
            if (error != null) {
                log.error { "Error updating instance(${request.node}), error(${error.message}" }

                responseObserver.onError(error)
            } else {
                log.info { "Updated instance(${request.cluster}/${request.node}), status()" }

                responseObserver.onCompleted()
            }
        }
    }
}

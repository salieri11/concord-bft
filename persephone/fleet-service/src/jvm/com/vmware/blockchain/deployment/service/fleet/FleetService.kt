/* **************************************************************************
 * Copyright (c) 2019 VMware, Inc.  All rights reserved. VMware Confidential
 * *************************************************************************/
package com.vmware.blockchain.deployment.service.fleet

import com.vmware.blockchain.deployment.logging.error
import com.vmware.blockchain.deployment.logging.info
import com.vmware.blockchain.deployment.logging.logger
import com.vmware.blockchain.deployment.model.FleetMessage
import com.vmware.blockchain.deployment.model.FleetServiceImplBase
import com.vmware.blockchain.deployment.model.InstanceMessage
import com.vmware.blockchain.deployment.model.UpdateInstanceRequest
import com.vmware.blockchain.deployment.model.UpdateInstanceResponse
import io.grpc.stub.StreamObserver
import kotlinx.coroutines.CoroutineScope
import kotlinx.coroutines.Dispatchers
import kotlinx.coroutines.Job
import kotlinx.coroutines.SupervisorJob
import kotlinx.coroutines.cancelAndJoin
import kotlinx.coroutines.launch
import kotlin.coroutines.CoroutineContext

/**
 * An implementation of [com.vmware.blockchain.deployment.model.FleetService].
 */
class FleetService(
    private val context: CoroutineContext = Dispatchers.Default
) : FleetServiceImplBase(), CoroutineScope {

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

    override fun createManagedSession(
        responseObserver: StreamObserver<FleetMessage>
    ): StreamObserver<InstanceMessage> {
        return super.createManagedSession(responseObserver)
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

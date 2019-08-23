/* **************************************************************************
 * Copyright (c) 2019 VMware, Inc. All rights reserved. VMware Confidential
 * **************************************************************************/
package com.vmware.blockchain.deployment.service.orchestrationsite

import com.vmware.blockchain.deployment.logging.info
import com.vmware.blockchain.deployment.logging.error
import com.vmware.blockchain.deployment.logging.logger
import com.vmware.blockchain.deployment.v1.ListOrchestrationSitesRequest
import com.vmware.blockchain.deployment.v1.ListOrchestrationSitesResponse
import com.vmware.blockchain.deployment.v1.OrchestrationSite
import com.vmware.blockchain.deployment.v1.OrchestrationSiteServiceImplBase
import com.vmware.blockchain.deployment.v1.OrchestrationSiteView
import io.grpc.stub.StreamObserver
import kotlinx.coroutines.CoroutineScope
import kotlinx.coroutines.Deferred
import kotlinx.coroutines.Dispatchers
import kotlinx.coroutines.Job
import kotlinx.coroutines.SupervisorJob
import kotlinx.coroutines.async
import kotlinx.coroutines.cancelAndJoin
import kotlinx.coroutines.launch
import kotlin.coroutines.CoroutineContext

/**
 * Implementation of OrchestrationSiteService service.
 */
class OrchestrationSiteService(
    private val context: CoroutineContext = Dispatchers.Default,
    private val orchestrations: List<OrchestrationSite>
) : OrchestrationSiteServiceImplBase(), CoroutineScope {

    /** Logging instance. */
    private val log by logger()

    /** [CoroutineContext] to launch all coroutines associated with this instance. */
    override val coroutineContext: CoroutineContext
        get() = context + job

    /** Parent [Job] of all coroutines associated with this instance's operation. */
    private val job: Job = SupervisorJob()

    suspend fun initialize() {
    }

    /**
     * Shutdown the service instance.
     */
    suspend fun shutdown() {
        job.cancelAndJoin()
    }

    fun initializeAsync(): Deferred<Unit> {
        return async {
            initialize()
        }
    }

    /**
     * Shutdown the service instance.
     *
     * @return
     *   an awaitable [Deferred] task signaling the completion of the shutdown operation.
     */
    fun shutdownAsync(): Deferred<Unit> {
        return async {
            shutdown()
        }
    }

    override fun listOrchestrationSites(
        request: ListOrchestrationSitesRequest,
        responseObserver: StreamObserver<ListOrchestrationSitesResponse>
    ) {
        // Each request is served in its own coroutine for 2 reasons:
        // 1. Responses can be sent in a back-pressure aware and non-blocking manner.
        // 2. Pagination session state can be tracked independently.
        //
        // The cost of this approach is higher memory footprint, especially if pagination support
        // is higher on memory cost (if pagination is serving content in a snapshot manner and
        // generating the listing involves querying external state (in the future).
        launch(coroutineContext) {
            // Site management is not yet dynamic, the implementations serves the static content.
            // TODO: Support pagination.
            val view = orchestrations
                    .map { OrchestrationSiteView(it.id, it.info.type, it.info.labels) }
            val response = ListOrchestrationSitesResponse(header = request.header, sites = view)

            log.info { "Listing request(${request.header.id}), ${view.size} sites returned" }
            responseObserver.onNext(response)
        }.invokeOnCompletion { error ->
            if (error != null) {
                log.error { "Error create orchestration site listing, error(${error.message})" }

                responseObserver.onError(error)
            } else {
                responseObserver.onCompleted()
            }
        }
    }
}

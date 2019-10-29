/* **************************************************************************
 * Copyright (c) 2019 VMware, Inc. All rights reserved. VMware Confidential
 * **************************************************************************/
package com.vmware.blockchain.deployment.service.orchestrationsite

import com.vmware.blockchain.deployment.logging.error
import com.vmware.blockchain.deployment.logging.info
import com.vmware.blockchain.deployment.logging.logger
import com.vmware.blockchain.deployment.orchestration.OrchestratorProvider
import com.vmware.blockchain.deployment.reactive.BaseSubscriber
import com.vmware.blockchain.deployment.service.grpc.support.toStatus
import com.vmware.blockchain.deployment.v1.OrchestrationSiteServiceImplBase
import com.vmware.blockchain.deployment.v1.ValidateOrchestrationSiteRequest
import com.vmware.blockchain.deployment.v1.ValidateOrchestrationSiteResponse
import io.grpc.Status
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
    private val orchestratorProvider: OrchestratorProvider
) : OrchestrationSiteServiceImplBase(), CoroutineScope {

    /** Logging instance. */
    private val log by logger()

    /** [CoroutineContext] to launch all coroutines associated with this instance. */
    override val coroutineContext: CoroutineContext
        get() = context + job

    /** Parent [Job] of all coroutines associated with this instance's operation. */
    private val job: Job = SupervisorJob()

    fun initialize() {
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

    override fun validateOrchestrationSite(
        request: ValidateOrchestrationSiteRequest,
        responseObserver: StreamObserver<ValidateOrchestrationSiteResponse>
    ) {
        launch(coroutineContext) {
            val newOrchestrator = orchestratorProvider.newOrchestrator(request.site)
            val publisher = newOrchestrator.validate()
            val subscriber = BaseSubscriber<Boolean>(
                    onNext = { result ->
                        if (result) {
                            val response = ValidateOrchestrationSiteResponse(
                                    header = request.header,
                                    site = request.site
                            )
                            responseObserver.onNext(response)

                            log.info {
                                "Validation request(${request.header.id}), " +
                                        "type(${request.site.type}, labels(${request.site.labels})"
                            }
                        } else {
                            responseObserver.onError(Status.INVALID_ARGUMENT.asException())
                        }
                    },
                    onComplete = { responseObserver.onCompleted() },
                    onError = { error ->
                        log.error { "Error validating orchestration site, error(${error.message})" }

                        responseObserver.onError(error.toStatus().asException())
                    }
            )

            // Subscribe to turn the workflow hot.
            publisher.subscribe(subscriber)
        }.invokeOnCompletion { error ->
            if (error != null) {
                log.error { "Error validating orchestration site, error(${error.message})" }

                responseObserver.onError(error)
            }
        }
    }
}

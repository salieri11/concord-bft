/* **************************************************************************
 * Copyright (c) 2019 VMware, Inc.  All rights reserved. VMware Confidential
 * *************************************************************************/
package com.vmware.blockchain.deployment.service.fleet

import com.vmware.blockchain.deployment.logging.error
import com.vmware.blockchain.deployment.logging.info
import com.vmware.blockchain.deployment.logging.warn
import com.vmware.blockchain.deployment.logging.logger
import com.vmware.blockchain.deployment.v1.ConcordClusterIdentifier
import com.vmware.blockchain.deployment.v1.ConcordNodeIdentifier
import com.vmware.blockchain.deployment.v1.FleetControlServiceImplBase
import com.vmware.blockchain.deployment.v1.FleetMessage
import com.vmware.blockchain.deployment.v1.InstanceMessage
import com.vmware.blockchain.grpc.kotlinx.serialization.ChannelStreamObserver
import io.grpc.stub.StreamObserver
import kotlin.coroutines.CoroutineContext
import kotlinx.coroutines.CoroutineScope
import kotlinx.coroutines.Dispatchers
import kotlinx.coroutines.Job
import kotlinx.coroutines.SupervisorJob
import kotlinx.coroutines.cancelAndJoin
import kotlinx.coroutines.channels.Channel
import kotlinx.coroutines.channels.ReceiveChannel
import kotlinx.coroutines.channels.SendChannel
import kotlinx.coroutines.launch

/**
 * An implementation of [com.vmware.blockchain.deployment.v1.FleetManagementService].
 */
class FleetControlService(
    private val context: CoroutineContext = Dispatchers.Default
) : FleetControlServiceImplBase(), CoroutineScope {

    data class SessionKey(val cluster: ConcordClusterIdentifier, val node: ConcordNodeIdentifier)

    class Session(
        val instance: String,
        val receiver: ReceiveChannel<InstanceMessage>,
        val sender: SendChannel<FleetMessage>
    ) {
        val command: Channel<String> = Channel()
    }

    /** Logging instance. */
    private val log by logger()

    /** [CoroutineContext] to launch all coroutines associated with this instance. */
    override val coroutineContext: CoroutineContext
        get() = context + job

    /** Parent [Job] of all coroutines associated with this instance's operation. */
    private val job: Job = SupervisorJob()

    /** Map of all active managed sessions. */
    private val sessions: Map<SessionKey, Session> = mutableMapOf()

    /**
     * Shutdown the service instance.
     */
    suspend fun shutdown() {
        job.cancelAndJoin()
    }

    override fun createManagedSession(
        responseObserver: StreamObserver<FleetMessage>
    ): StreamObserver<InstanceMessage> {
        // Create a receive-channel based stream observer.
        val receiver = ChannelStreamObserver<InstanceMessage>()

        // Keep each managed session in its own scoped coroutine.
        launch(coroutineContext) {
            // The initial message must be Type.CREATE_SESSION_REQUEST, everything else is an error.
            val request = receiver.asReceiveChannel().receive()
            if (request.type == InstanceMessage.Type.CREATE_SESSION_REQUEST) {
                // Send the initial response back to acknowledge the session request.
                responseObserver.onNext(
                        FleetMessage(
                                type = FleetMessage.Type.CREATE_SESSION_RESPONSE,
                                sessionResponse = FleetMessage.CreateSessionResponse(
                                        status = FleetMessage.CreateSessionResponse.Status.OK
                                )
                        )
                )

                // Process messages until connection closure.
                for (message in receiver.asReceiveChannel()) {
                    when (message.type) {
                        InstanceMessage.Type.STATUS -> {
                            log.info { "Received status(${message.status.state})" }

                            responseObserver.onNext(
                                    FleetMessage(type = FleetMessage.Type.GET_CONFIGURATION)
                            )
                        }
                        InstanceMessage.Type.CONFIGURATION -> {
                            log.info { "Received configuration(${message.configuration})" }
                        }
                        else -> log.warn { "Message type(${message.type}) ignored" }
                    }
                }

                log.info { "Closing session created by request(${request.header.id}" }
            } else {
                responseObserver.onNext(
                        FleetMessage(
                                type = FleetMessage.Type.CREATE_SESSION_RESPONSE,
                                sessionResponse = FleetMessage.CreateSessionResponse(
                                        status = FleetMessage.CreateSessionResponse.Status.FAILED
                                )
                        )
                )

                log.warn { "Request(${request.header.id}) sent ${request.type} as initial message" }
            }
        }.invokeOnCompletion { error ->
            if (error != null) {
                log.error { "Unhandled error during session, error(${error.message})" }

                responseObserver.onError(error)
            } else {
                // Coroutine exited, so the managed session is now done.
                responseObserver.onCompleted()
            }
        }

        return receiver
    }
}

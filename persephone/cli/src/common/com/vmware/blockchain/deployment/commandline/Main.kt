/* **************************************************************************
 * Copyright (c) 2019 VMware, Inc.  All rights reserved. VMware Confidential
 * *************************************************************************/
package com.vmware.blockchain.deployment.commandline

import com.vmware.blockchain.deployment.logging.Logger
import com.vmware.blockchain.deployment.logging.info
import com.vmware.blockchain.deployment.logging.logger
import com.vmware.blockchain.deployment.model.ConcordClusterIdentifier
import com.vmware.blockchain.deployment.model.ConcordComponent
import com.vmware.blockchain.deployment.model.ConcordModelSpecification
import com.vmware.blockchain.deployment.model.ConcordNodeIdentifier
import com.vmware.blockchain.deployment.model.core.URI
import com.vmware.blockchain.deployment.model.core.UUID
import com.vmware.blockchain.deployment.model.ethereum.Genesis
import com.vmware.blockchain.deployment.orchestration.getOrchestrationSiteMapSerializer
import com.vmware.blockchain.deployment.orchestration.Orchestrator
import com.vmware.blockchain.deployment.reactive.BaseSubscriber
import com.vmware.blockchain.deployment.vmc.VmcOrchestrator
import kotlinx.coroutines.CompletableDeferred
import kotlinx.coroutines.CoroutineName
import kotlinx.coroutines.Dispatchers
import kotlinx.coroutines.coroutineScope
import kotlinx.coroutines.reactive.awaitSingle
import kotlinx.coroutines.runBlocking
import kotlinx.serialization.json.Json
import kotlinx.serialization.json.JsonConfiguration
import kotlin.system.exitProcess

/**
 * Command-line entry-point for deployment service.
 *
 * This file servers as placeholder for what a command-line utility will eventually utilize. Right
 * now this is simply a scratch pad offering convenient location to tweak/test against live service
 * deployments, as well as an example of a deployment workflow.
 */
fun main(args: Array<String>) {
    val log: Logger = logger(VmcOrchestrator::class)

    if (args.isEmpty()) {
        log.info { "Configuration not given as argument" }
        exitProcess(0)
    }

    // Parse argument for orchestration site info (fetch the first available entry).
    val sites = Json(JsonConfiguration.Stable).parse(getOrchestrationSiteMapSerializer(), args[0])
    require(sites.values.isNotEmpty())
    val site = sites.values.first()

    runBlocking(Dispatchers.IO + CoroutineName("ProvisioningDispatcher")) {
        coroutineScope {
            val orchestrator = requireNotNull(
                    VmcOrchestrator.newOrchestrator(site, coroutineContext).awaitSingle()
            )

            // Create compute.
            val genesis = Genesis(
                    config = Genesis.Config(
                            chainId = 1,
                            homesteadBlock = 0,
                            eip155Block = 0,
                            eip158Block = 0
                    ),
                    nonce = "0x0000000000000000",
                    difficulty = "0x400",
                    mixhash = "0x0000000000000000000000000000000000000000000000000000000000000000",
                    parentHash = "0x0000000000000000000000000000000000000000000000000000000000000000",
                    gasLimit = "0xf4240",
                    alloc = mapOf(
                            "262c0d7ab5ffd4ede2199f6ea793f819e1abb019" to Genesis.Wallet("12345"),
                            "5bb088f57365907b1840e45984cae028a82af934" to Genesis.Wallet("0xabcdef"),
                            "0000a12b3f3d6c9b0d3f126a83ec2dd3dad15f39" to Genesis.Wallet("0x7fffffffffffffff")
                    )
            )
            val computeCreate = Orchestrator.CreateComputeResourceRequest(
                    UUID.randomUUID().let {
                        ConcordClusterIdentifier(it.mostSignificantBits, it.leastSignificantBits)
                    },
                    UUID.randomUUID().let {
                        ConcordNodeIdentifier(it.mostSignificantBits, it.leastSignificantBits)
                    },
                    ConcordModelSpecification(
                            version = "photon-3.0-64",
                            // template = "5b7eac22-976e-47fa-a000-1e09020a1c5d",
                            template = "8abc7fda-9576-4b13-9beb-06f867cf2c7c",
                            components = listOf(
                                    ConcordComponent(
                                            ConcordComponent.Type.DOCKER_IMAGE,
                                            "vmwblockchain/concord-core:latest"
                                    ),
                                    ConcordComponent(
                                            ConcordComponent.Type.DOCKER_IMAGE,
                                            "vmwblockchain/ethrpc:latest"
                                    ),
                                    ConcordComponent(
                                            ConcordComponent.Type.DOCKER_IMAGE,
                                            "vmwblockchain/agent-testing:latest"
                                    )
                            )
                    ),
                    genesis
            )
            val computeCreated = CompletableDeferred<URI>()
            val computeStarted = CompletableDeferred<URI>()
            val computeCreateSubscriber = BaseSubscriber<Orchestrator.ComputeResourceEvent>(
                    onNext = {
                        when (it) {
                            is Orchestrator.ComputeResourceEvent.Created ->
                                computeCreated.complete(it.resource)
                            is Orchestrator.ComputeResourceEvent.Started ->
                                computeStarted.complete(it.resource)
                            else -> Unit
                        }
                    },
                    onComplete = {
                        log.info { "Compute completed" }
                    },
                    onError = { computeCreated.completeExceptionally(it) }
            )
            orchestrator.createDeployment(computeCreate).subscribe(computeCreateSubscriber)

            // Create network.
            val networkCreate = Orchestrator.CreateNetworkResourceRequest("test-node", true)
            val networkCreated = CompletableDeferred<URI>()
            val networkCreateSubscriber = BaseSubscriber<Orchestrator.NetworkResourceEvent>(
                    onNext = {
                        when (it) {
                            is Orchestrator.NetworkResourceEvent.Created ->
                                if (it.public) {
                                    networkCreated.complete(it.resource)
                                }
                            else -> Unit
                        }
                    },
                    onError = { networkCreated.completeExceptionally(it) }
            )
            orchestrator.createNetworkAddress(networkCreate).subscribe(networkCreateSubscriber)

            log.info { "Created network resource: ${networkCreated.await()}" }
            log.info { "Created compute resource: ${computeCreated.await()}" }
            log.info { "Started compute resource: ${computeStarted.await()}" }

            val networkAllocate = Orchestrator.CreateNetworkAllocationRequest(
                    "test-node",
                    computeCreated.await(),
                    networkCreated.await()
            )
            val networkAllocated = CompletableDeferred<URI>()
            val networkAllocateSubscriber = BaseSubscriber<Orchestrator.NetworkAllocationEvent>(
                    onNext = {
                        when (it) {
                            is Orchestrator.NetworkAllocationEvent.Created ->
                                networkAllocated.complete(it.resource)
                            else -> Unit
                        }
                    },
                    onError = { networkAllocated.completeExceptionally(it) }
            )
            orchestrator.createNetworkAllocation(networkAllocate)
                    .subscribe(networkAllocateSubscriber)

            log.info { "Network allocation: ${networkAllocated.await()}" }

            // Teardown.
            val networkDeallocate =
                    Orchestrator.DeleteNetworkAllocationRequest(networkAllocated.await())
            val networkDeallocated = CompletableDeferred<URI>()
            val networkDeallocateSubscriber = BaseSubscriber<Orchestrator.NetworkAllocationEvent>(
                    onNext = {
                        when (it) {
                            is Orchestrator.NetworkAllocationEvent.Deleted ->
                                networkDeallocated.complete(it.resource)
                            else -> Unit
                        }
                    },
                    onError = { networkDeallocated.completeExceptionally(it) }
            )
            orchestrator.deleteNetworkAllocation(networkDeallocate)
                    .subscribe(networkDeallocateSubscriber)

            log.info { "Deleted network allocation: ${networkDeallocated.await()}" }

            val computeDelete = Orchestrator.DeleteComputeResourceRequest(computeCreated.await())
            val computeDeleted = CompletableDeferred<URI>()
            val computeDeleteSubscriber = BaseSubscriber<Orchestrator.ComputeResourceEvent>(
                    onNext = {
                        when (it) {
                            is Orchestrator.ComputeResourceEvent.Deleted ->
                                computeDeleted.complete(it.resource)
                            else -> Unit
                        }
                    },
                    onError = { computeDeleted.completeExceptionally(it) }
            )
            orchestrator.deleteDeployment(computeDelete).subscribe(computeDeleteSubscriber)

            val networkDelete = Orchestrator.DeleteNetworkResourceRequest(networkCreated.await())
            val networkDeleted = CompletableDeferred<URI>()
            val networkDeleteSubscriber = BaseSubscriber<Orchestrator.NetworkResourceEvent>(
                    onNext = {
                        when (it) {
                            is Orchestrator.NetworkResourceEvent.Deleted ->
                                networkDeleted.complete(it.resource)
                            else -> Unit
                        }
                    },
                    onError = { networkDeleted.completeExceptionally(it) }
            )
            orchestrator.deleteNetworkAddress(networkDelete).subscribe(networkDeleteSubscriber)

            log.info { "Deleted compute resource: ${computeDeleted.await()}" }
            log.info { "Deleted network resource: ${networkDeleted.await()}" }
        }
    }

    log.info { "SddcProvisioning tested" }
}

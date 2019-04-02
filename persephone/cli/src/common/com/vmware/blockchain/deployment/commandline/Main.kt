/* **************************************************************************
 * Copyright (c) 2019 VMware, Inc.  All rights reserved. VMware Confidential
 * *************************************************************************/
package com.vmware.blockchain.deployment.commandline

import com.vmware.blockchain.deployment.logging.Logger
import com.vmware.blockchain.deployment.logging.info
import com.vmware.blockchain.deployment.logging.logger
import com.vmware.blockchain.deployment.model.ConcordClusterIdentifier
import com.vmware.blockchain.deployment.model.ConcordModelSpecification
import com.vmware.blockchain.deployment.model.ConcordNodeIdentifier
import com.vmware.blockchain.deployment.model.core.BearerTokenCredential
import com.vmware.blockchain.deployment.model.core.Credential
import com.vmware.blockchain.deployment.model.core.Endpoint
import com.vmware.blockchain.deployment.model.core.URI
import com.vmware.blockchain.deployment.model.core.UUID
import com.vmware.blockchain.deployment.model.orchestration.OrchestrationSiteInfo
import com.vmware.blockchain.deployment.orchestration.Orchestrator
import com.vmware.blockchain.deployment.reactive.BaseSubscriber
import com.vmware.blockchain.deployment.vmc.VmcOrchestrator
import kotlinx.coroutines.CompletableDeferred
import kotlinx.coroutines.CoroutineName
import kotlinx.coroutines.Dispatchers
import kotlinx.coroutines.coroutineScope
import kotlinx.coroutines.reactive.awaitSingle
import kotlinx.coroutines.runBlocking

/**
 * Command-line entry-point for deployment service.
 *
 * TODO:
 * This file servers as placeholder for what a command-line utility will eventually utilize. Right
 * now this is simply a scratch pad offering convenient location to tweak/test against live service
 * deployments, as well as an example of a deployment workflow.
 */
fun main() {
    val log: Logger = logger(VmcOrchestrator::class)
    val token = BearerTokenCredential("change-me-token")
    val orgId = "change-me-org-id"
    val datacenterId = "change-me-dc-id"
    val site = OrchestrationSiteInfo(
            OrchestrationSiteInfo.Type.VMC,
            OrchestrationSiteInfo.Vmc(
                    authentication = Endpoint(
                            URI("https://console.cloud.vmware.com"),
                            Credential(Credential.Type.BEARER, tokenCredential = token)
                    ),
                    api = Endpoint(URI("https://vmc.vmware.com"), Credential()),
                    organization = orgId,
                    datacenter = datacenterId
            )
    )

    runBlocking(Dispatchers.IO + CoroutineName("ProvisioningDispatcher")) {
        coroutineScope {
            val orchestrator = requireNotNull(
                    VmcOrchestrator.newOrchestrator(site, coroutineContext).awaitSingle()
            )

            val request = Orchestrator.CreateComputeResourceRequest(
                    UUID.randomUUID().let {
                        ConcordClusterIdentifier(it.mostSignificantBits, it.leastSignificantBits)
                    },
                    UUID.randomUUID().let {
                        ConcordNodeIdentifier(it.mostSignificantBits, it.leastSignificantBits)
                    },
                    ConcordModelSpecification(
                            version = "123",
                            template = "photon-2.0"
                    )
            )

            val created = CompletableDeferred<URI>()
            val started = CompletableDeferred<URI>()
            val resultSubscriber = BaseSubscriber<Orchestrator.ComputeResourceEvent>(
                    onNext = {
                        when (it) {
                            is Orchestrator.ComputeResourceEvent.Created ->
                                created.complete(it.resource)
                            is Orchestrator.ComputeResourceEvent.Started ->
                                started.complete(it.resource)
                            else -> Unit
                        }
                    },
                    onError = { created.completeExceptionally(it) }
            )
            orchestrator.createDeployment(request).subscribe(resultSubscriber)

            log.info { "Created instance: ${created.await()}" }
            log.info { "Started instance: ${started.await()}" }

            // Teardown.
            //    launch {
            //        log.vmc { "Deleting Control Networks" }
            //        orchestrator.deleteLogicalNetwork(createControlNetwork.await())
            //    }
            //    launch {
            //        log.vmc { "Deleting Replica Networks" }
            //        orchestrator.deleteLogicalNetwork(createReplicaNetwork.await())
            //    }
        }
    }

    log.info { "SddcProvisioning tested" }
}
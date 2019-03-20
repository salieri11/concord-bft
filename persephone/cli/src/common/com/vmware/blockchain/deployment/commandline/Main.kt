/* **************************************************************************
 * Copyright (c) 2019 VMware, Inc.  All rights reserved. VMware Confidential
 * *************************************************************************/
package com.vmware.blockchain.deployment.commandline

import com.vmware.blockchain.deployment.logging.Logger
import com.vmware.blockchain.deployment.logging.info
import com.vmware.blockchain.deployment.logging.logger
import com.vmware.blockchain.deployment.model.ConcordClusterIdentifier
import com.vmware.blockchain.deployment.model.ConcordModelSpecification
import com.vmware.blockchain.deployment.model.DeploymentSessionIdentifier
import com.vmware.blockchain.deployment.model.core.BearerTokenCredential
import com.vmware.blockchain.deployment.model.core.Credential
import com.vmware.blockchain.deployment.model.core.Endpoint
import com.vmware.blockchain.deployment.model.core.URI
import com.vmware.blockchain.deployment.model.core.UUID
import com.vmware.blockchain.deployment.model.orchestration.OrchestrationSite
import com.vmware.blockchain.deployment.model.orchestration.VmcOrchestrationSite
import com.vmware.blockchain.deployment.orchestration.Orchestrator
import com.vmware.blockchain.deployment.reactive.BaseSubscriber
import com.vmware.blockchain.deployment.vmc.VmcOrchestrator
import kotlinx.coroutines.CompletableDeferred
import kotlinx.coroutines.CoroutineName
import kotlinx.coroutines.Dispatchers
import kotlinx.coroutines.coroutineScope
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
    val site = OrchestrationSite(
            OrchestrationSite.Type.VMC,
            OrchestrationSite.Info.Vmc(
                    VmcOrchestrationSite(
                            authentication = Endpoint(
                                    URI("https://console.cloud.vmware.com"),
                                    Credential(Credential.Type.BEARER, tokenCredential = token)
                            ),
                            api = Endpoint(URI("https://vmc.vmware.com"), Credential()),
                            organization = "change-me-org-id",
                            datacenter = "change-me-dc-id"
                    )
            )
    )

    runBlocking(Dispatchers.IO + CoroutineName("ProvisioningDispatcher")) {
        coroutineScope {
            val orchestrator = requireNotNull(
                    VmcOrchestrator.newOrchestrator(site, coroutineContext).await()
            )

            val request = Orchestrator.CreateComputeResourceRequest(
                    UUID.randomUUID().let {
                        DeploymentSessionIdentifier(it.mostSignificantBits, it.leastSignificantBits)
                    },
                    UUID.randomUUID().let {
                        ConcordClusterIdentifier(it.mostSignificantBits, it.leastSignificantBits)
                    },
                    ConcordModelSpecification(
                            version = "123",
                            template = "photon-2.0"
                    )
            )

            val created = CompletableDeferred<URI>()
            val started = CompletableDeferred<URI>()
            val resultSubscriber = BaseSubscriber<Orchestrator.DeploymentEvent>(
                    onNext = {
                        when (it) {
                            is Orchestrator.DeploymentEvent.Created ->
                                created.complete(it.resourceIdentifier)
                            is Orchestrator.DeploymentEvent.Started ->
                                started.complete(it.resourceIdentifier)
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
            //        log.info { "Deleting Control Networks" }
            //        orchestrator.deleteLogicalNetwork(createControlNetwork.await())
            //    }
            //    launch {
            //        log.info { "Deleting Replica Networks" }
            //        orchestrator.deleteLogicalNetwork(createReplicaNetwork.await())
            //    }
        }
    }

    log.info { "SddcProvisioning tested" }
}
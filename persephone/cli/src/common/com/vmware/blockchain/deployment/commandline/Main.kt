/* **************************************************************************
 * Copyright (c) 2019 VMware, Inc.  All rights reserved. VMware Confidential
 * *************************************************************************/
package com.vmware.blockchain.deployment.commandline

import com.vmware.blockchain.deployment.logging.Logger
import com.vmware.blockchain.deployment.logging.info
import com.vmware.blockchain.deployment.logging.logger
import com.vmware.blockchain.deployment.model.OrchestrationSite
import com.vmware.blockchain.deployment.model.VmcOrchestrationSite
import com.vmware.blockchain.deployment.model.core.BearerTokenCredential
import com.vmware.blockchain.deployment.model.core.Credential
import com.vmware.blockchain.deployment.model.core.Endpoint
import com.vmware.blockchain.deployment.model.core.URI
import com.vmware.blockchain.deployment.vmc.Orchestrator
import kotlinx.coroutines.CoroutineName
import kotlinx.coroutines.Dispatchers
import kotlinx.coroutines.async
import kotlinx.coroutines.coroutineScope
import kotlinx.coroutines.launch
import kotlinx.coroutines.runBlocking
import kotlin.random.Random

/**
 * Command-line entry-point for deployment service.
 *
 * TODO(jameschang):
 * This file servers as placeholder for what a command-line utility will eventually utilize. Right
 * now this is simply a scratch pad offering convenient location to tweak/test against live service
 * deployments, as well as an example of a deployment workflow.
 */
fun main() {
    val log: Logger = logger(Orchestrator::class)
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

    val blockchainId = "testchain"
    runBlocking(Dispatchers.IO + CoroutineName("ProvisioningDispatcher")) {
        coroutineScope {
            val provisioner = requireNotNull(Orchestrator.newOrchestrator(site, coroutineContext).await())

            val getFolder = async { provisioner.getFolder() }
            val getDatastore = async { provisioner.getDatastore() }
            val getResourcePool = async { provisioner.getResourcePool() }
            val createControlNetwork = async {
                provisioner.ensureLogicalNetwork("cgw", "sddc-cgw-vpn", 0x0AC00000, 24)
            }
            val createReplicaNetwork = async {
                provisioner.ensureLogicalNetwork("cgw", "$blockchainId-data", 0x0AC00000, 24)
            }
            val getLibraryItem = async { provisioner.getLibraryItem("photon-2.0") }

            provisioner.getNetworkSegment("cgw", "sddc-cgw-vpn").also { log.info { "Logical ControlNetwork: $it" }}
            provisioner.getNetworkSegment("cgw", "$blockchainId-data").also { log.info { "Logical DataNetwork: $it" }}

            // Collect all information and deploy.
            launch {
                val folder = getFolder.await().also { log.info { "Folder: $it" } }
                val datastore = getDatastore.await().also { log.info { "Datastore: $it" } }
                val resourcePool = getResourcePool.await().also { log.info { "ResourcePool: $it" } }
                val controlNetwork = createControlNetwork.await().also { log.info { "ControlNetwork: $it" }}
                val dataNetwork = createReplicaNetwork.await().also { log.info { "DataNetwork: $it" }}
                val libraryItem = getLibraryItem.await().also { log.info { "LibraryItem: $it" }}
                val instance = provisioner.createInstance(
                        instanceName = "$blockchainId-replica${Random.nextInt(0, 100)}",
                        libraryItem = requireNotNull(libraryItem),
                        datastore = requireNotNull(datastore),
                        resourcePool = requireNotNull(resourcePool),
                        folder = requireNotNull(folder),
                        controlNetwork = controlNetwork,
                        dataNetwork = dataNetwork)
                log.info { "Created instance: $instance" }
            }

            // Teardown.
//                launch {
//                    log.info { "Deleting Control Networks" }
//                    provisioner.deleteLogicalNetwork(createControlNetwork.await())
//                }
//                launch {
//                    log.info { "Deleting Replica Networks" }
//                    provisioner.deleteLogicalNetwork(createReplicaNetwork.await())
//                }
        }
    }

    log.info { "SddcProvisioning tested" }
}
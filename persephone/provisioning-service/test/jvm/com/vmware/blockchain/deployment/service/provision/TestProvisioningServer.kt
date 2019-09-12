/* **************************************************************************
 * Copyright (c) 2019 VMware, Inc. All rights reserved. VMware Confidential
 * **************************************************************************/
package com.vmware.blockchain.deployment.service.provision

import com.vmware.blockchain.deployment.service.orchestrationsite.OrchestrationSiteService
import com.vmware.blockchain.deployment.service.orchestrationsite.OrchestrationSiteServiceModule
import com.vmware.blockchain.deployment.v1.Credential
import com.vmware.blockchain.deployment.v1.Endpoint
import com.vmware.blockchain.deployment.v1.IPv4Network
import com.vmware.blockchain.deployment.v1.OrchestrationSite
import com.vmware.blockchain.deployment.v1.OrchestrationSiteIdentifier
import com.vmware.blockchain.deployment.v1.OrchestrationSiteInfo
import com.vmware.blockchain.deployment.v1.VSphereDatacenterInfo
import com.vmware.blockchain.deployment.v1.VmcOrchestrationSiteInfo
import dagger.BindsInstance
import dagger.Component
import io.grpc.Server
import javax.inject.Singleton

/**
 * Parallel server construction of [ProvisioningServer] for unit-tests.
 */
@Component(modules = [
    ProvisioningServiceModule::class,
    TestConfigurationServiceModule::class,
    OrchestrationSiteServiceModule::class,
    TestOrchestratorModule::class,
    TestEndpointProviderModule::class
])
@Singleton
interface TestProvisioningServer {

    /** Singleton service instance for managing orchestration sites.  */
    fun orchestrationSiteService(): OrchestrationSiteService

    /** Singleton service instance for provisioning Concord clusters.  */
    fun provisionService(): ProvisioningService

    /** Singleton server instance hosting a test configuration service instance. */
    fun configurationServiceServer(): Server

    /**
     * Builder class for testing.
     */
    @Component.Builder
    interface Builder {

        @BindsInstance
        fun orchestrations(entries: List<OrchestrationSite>): Builder

        fun build(): TestProvisioningServer
    }
}

/**
 * Create a new set of mappings that denote all available orchestration sites, for
 * facilitating unit-testing of [TestProvisioningServer]-hosted services.
 *
 * @return
 *   a mapping of [OrchestrationSiteIdentifier] to [OrchestrationSiteInfo].
 */
fun newOrchestrationSites(): List<OrchestrationSite> {
    return listOf(
            OrchestrationSite(
                    OrchestrationSiteIdentifier(1, 0),
                    newOrchestrationSiteInfo(1)
            ),
            OrchestrationSite(
                    OrchestrationSiteIdentifier(2, 0),
                    newOrchestrationSiteInfo(2)
            ),
            OrchestrationSite(
                    OrchestrationSiteIdentifier(3, 0),
                    newOrchestrationSiteInfo(3)
            ),
            OrchestrationSite(
                    OrchestrationSiteIdentifier(4, 0),
                    newOrchestrationSiteInfo(4)
            )
    )
}

/**
 * Create a mock [OrchestrationSiteInfo] instance based on an integer parameter, for
 * facilitating unit-testing of [TestProvisioningServer]-hosted services.
 *
 * @param[i]
 *   integer value to be stamped into the orchestration site's various identifiers.
 *
 * @return
 *   a new instance of [OrchestrationSiteInfo].
 */
private fun newOrchestrationSiteInfo(i: Int): OrchestrationSiteInfo {
    val apiEndpoint = "https://apiserver-$i"
    val datacenter = "datacenter-$i"
    return OrchestrationSiteInfo(
            OrchestrationSiteInfo.Type.VMC,
            mapOf("site-name" to "site-$i"),
            VmcOrchestrationSiteInfo(
                    Endpoint("https://authserver", Credential()),
                    Endpoint(apiEndpoint, Credential()),
                    Endpoint("https://registry", Credential()),
                    "test-org",
                    datacenter,
                    VSphereDatacenterInfo(
                        "resource-pool",
                        "datastore",
                        "vm-folder",
                        IPv4Network("vmware-vpn")
                    )
            )
    )
}

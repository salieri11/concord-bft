/* **************************************************************************
 * Copyright (c) 2019 VMware, Inc. All rights reserved. VMware Confidential
 * **************************************************************************/
package com.vmware.blockchain.services.provision

import com.vmware.blockchain.deployment.services.orchestrationsite.OrchestrationSiteService
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
    com.vmware.blockchain.services.provision.TestConfigurationServiceModule::class,
    OrchestrationSiteServiceModule::class,
    com.vmware.blockchain.services.provision.TestOrchestratorModule::class,
    com.vmware.blockchain.services.provision.TestEndpointProviderModule::class
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

        @BindsInstance
        fun orchestrationSiteValidator(validator: OrchestrationSiteValidator): Builder

        fun build(): TestProvisioningServer
    }
}

/**
 * Functional type that encapsulates a validation function to validate [OrchestrationSiteInfo].
 */
interface OrchestrationSiteValidator {

    /**
     * A default implementation of [OrchestrationSiteValidator] that always returns `true` for every
     * validation call.
     */
    object DefaultValidator : OrchestrationSiteValidator {
        override fun test(site: OrchestrationSiteInfo): Boolean = true
    }

    /**
     * Performs the validation against the given [OrchestrationSiteInfo] instance.
     *
     * @param[site]
     *   site to be validated.
     *
     * @return
     *   `true` if validated successfully, `false` otherwise.
     */
    fun test(site: OrchestrationSiteInfo): Boolean
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
 * @param[index]
 *   integer value to be stamped into the orchestration site's various identifiers.
 *
 * @return
 *   a new instance of [OrchestrationSiteInfo].
 */
fun newOrchestrationSiteInfo(
    index: Int,
    labels: Map<String, String> = emptyMap()
): OrchestrationSiteInfo {
    val apiEndpoint = "https://apiserver-$index"
    val datacenter = "datacenter-$index"
    val allLabels = labels + ("site-name" to "site-$index")
    return OrchestrationSiteInfo(
            OrchestrationSiteInfo.Type.VMC,
            allLabels,
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

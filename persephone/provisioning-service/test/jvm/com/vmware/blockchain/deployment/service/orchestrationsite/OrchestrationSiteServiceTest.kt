/* **************************************************************************
 * Copyright (c) 2019 VMware, Inc. All rights reserved. VMware Confidential
 * **************************************************************************/
package com.vmware.blockchain.deployment.service.orchestrationsite

import com.vmware.blockchain.deployment.service.provision.DaggerTestProvisioningServer
import com.vmware.blockchain.deployment.service.provision.OrchestrationSiteValidator
import com.vmware.blockchain.deployment.service.provision.newOrchestrationSiteInfo
import com.vmware.blockchain.deployment.service.provision.newOrchestrationSites
import com.vmware.blockchain.deployment.v1.ListOrchestrationSitesRequest
import com.vmware.blockchain.deployment.v1.ListOrchestrationSitesResponse
import com.vmware.blockchain.deployment.v1.MessageHeader
import com.vmware.blockchain.deployment.v1.OrchestrationSite
import com.vmware.blockchain.deployment.v1.OrchestrationSiteIdentifier
import com.vmware.blockchain.deployment.v1.OrchestrationSiteInfo
import com.vmware.blockchain.deployment.v1.ValidateOrchestrationSiteRequest
import com.vmware.blockchain.deployment.v1.ValidateOrchestrationSiteResponse
import com.vmware.blockchain.grpc.kotlinx.serialization.ChannelStreamObserver
import io.grpc.Status
import io.grpc.StatusException
import kotlinx.coroutines.runBlocking
import org.assertj.core.api.Assertions
import org.junit.jupiter.api.Test

/**
 * Various test verifying functionality and semantics of [OrchestrationSiteService].
 */
class OrchestrationSiteServiceTest {

    companion object {
        const val FAULT_INJECT_VALIDATION_FAIL = "VALIDATION_FAIL"
    }

    /**
     * Create a new [OrchestrationSiteService].
     *
     * @return
     * a newly created [OrchestrationSiteService] instance.
     */
    private fun newOrchestrationSiteService(
        orchestrations: List<OrchestrationSite>
    ): OrchestrationSiteService {
        val validator = object : OrchestrationSiteValidator {
            override fun test(site: OrchestrationSiteInfo): Boolean {
                return !(site.labels.containsKey(FAULT_INJECT_VALIDATION_FAIL))
            }
        }

        return DaggerTestProvisioningServer.builder()
                .orchestrations(orchestrations)
                .orchestrationSiteValidator(validator)
                .build()
                .orchestrationSiteService()
    }

    /**
     * Create a new [OrchestrationSite].
     *
     * @param[index]
     *   value to set for [OrchestrationSiteIdentifier]'s low bits.
     * @param[labels]
     *   labels to set in [OrchestrationSite.info].
     *
     * @return
     *   a newly created [OrchestrationSite] instance.
     */
    private fun newOrchestrationSite(index: Int, labels: Map<String, String>): OrchestrationSite {
        return OrchestrationSite(
                OrchestrationSiteIdentifier(low = index.toLong()),
                newOrchestrationSiteInfo(index, labels)
        )
    }

    /**
     * Test that [OrchestrationSiteService.listOrchestrationSites] returns view information matching
     * the information that the server and service was loaded with.
     */
    @Test
    fun listResultViewMatchesInfo() {
        val orchestrations = newOrchestrationSites()
        val siteMap = orchestrations.associateBy({ it.id }, { it.info })

        runBlocking {
            // Prepare the environment.
            val service = newOrchestrationSiteService(orchestrations)
            service.initialize()

            val observer = ChannelStreamObserver<ListOrchestrationSitesResponse>(1)
            service.listOrchestrationSites(
                    ListOrchestrationSitesRequest(
                        header = MessageHeader(id = "listResultViewMatchesInfo")
                    ),
                    observer
            )

            val response = observer.asReceiveChannel().receive()
            for (site in response.sites) {
                Assertions.assertThat(site.id).isIn(siteMap.keys)
                Assertions.assertThat(site.type).isEqualTo(siteMap[site.id]?.type)
                Assertions.assertThat(site.labels).isEqualTo(siteMap[site.id]?.labels)
            }

            // Clean up.
            service.shutdown()
        }
    }

    /**
     * Test that [OrchestrationSiteService.validateOrchestrationSite] can process a successful
     * validation workflow (i.e. nothing in the site info that would trigger a failure should result
     * in a successful RPC call)
     */
    @Test
    fun validateOrchestrationSite() {
        val orchestrations = newOrchestrationSites()

        runBlocking {
            // Prepare the environment.
            val service = newOrchestrationSiteService(orchestrations)
            service.initialize()

            val observer = ChannelStreamObserver<ValidateOrchestrationSiteResponse>(1)
            val request = ValidateOrchestrationSiteRequest(
                    header = MessageHeader(id = "validateOrchestrationSite"),
                    site = OrchestrationSiteInfo()
            )
            service.validateOrchestrationSite(request, observer)
            val response = observer.asReceiveChannel().receive()

            // Verification.
            Assertions.assertThat(response.header).isEqualTo(request.header)

            // Clean up.
            service.shutdown()
        }
    }

    /**
     * Test that [OrchestrationSiteService.validateOrchestrationSite] can process a failure
     * validation workflow induced by fault-injection.
     */
    @Test
    fun validateInvalidOrchestrationSite() {
        val orchestrations = newOrchestrationSites()

        runBlocking {
            // Prepare the environment.
            val service = newOrchestrationSiteService(orchestrations)
            service.initialize()

            val observer = ChannelStreamObserver<ValidateOrchestrationSiteResponse>(1)
            service.validateOrchestrationSite(
                    ValidateOrchestrationSiteRequest(
                            header = MessageHeader(id = "validateInvalidOrchestrationSite"),
                            site = OrchestrationSiteInfo(
                                    labels = mapOf(FAULT_INJECT_VALIDATION_FAIL to "")
                            )
                    ),
                    observer
            )

            try {
                observer.asReceiveChannel().receive()
                Assertions.failBecauseExceptionWasNotThrown<StatusException>(
                        StatusException::class.java
                )
            } catch (error: StatusException) {
                Assertions.assertThat(error.status).isEqualTo(Status.INVALID_ARGUMENT)
            } finally {
                // Clean up.
                service.shutdown()
            }
        }
    }
}

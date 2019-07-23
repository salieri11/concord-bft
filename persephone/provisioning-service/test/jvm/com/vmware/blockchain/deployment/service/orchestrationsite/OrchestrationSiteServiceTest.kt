/* **************************************************************************
 * Copyright (c) 2019 VMware, Inc. All rights reserved. VMware Confidential
 * **************************************************************************/
package com.vmware.blockchain.deployment.service.orchestrationsite

import com.vmware.blockchain.deployment.model.Endpoint
import com.vmware.blockchain.deployment.model.ListOrchestrationSitesRequest
import com.vmware.blockchain.deployment.model.ListOrchestrationSitesResponse
import com.vmware.blockchain.deployment.model.MessageHeader
import com.vmware.blockchain.deployment.model.OrchestrationSite
import com.vmware.blockchain.deployment.service.provision.DaggerTestProvisioningServer
import com.vmware.blockchain.deployment.service.provision.newOrchestrationSites
import com.vmware.blockchain.grpc.kotlinx.serialization.ChannelStreamObserver
import kotlinx.coroutines.runBlocking
import org.assertj.core.api.Assertions
import org.junit.jupiter.api.Test

/**
 * Various test verifying functionality and semantics of [OrchestrationSiteService].
 */
class OrchestrationSiteServiceTest {

    /**
     * Create a new [OrchestrationSiteService].
     *
     * @return
     * a newly created [OrchestrationSiteService] instance.
     */
    private fun newOrchestrationSiteService(
        orchestrations: List<OrchestrationSite>
    ): OrchestrationSiteService {
        return DaggerTestProvisioningServer.builder()
                .orchestrations(orchestrations)
                .configServiceStub(Endpoint.defaultValue)
                .build()
                .orchestrationSiteService()
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
}

/*
 * Copyright (c) 2019 VMware, Inc. All rights reserved. VMware Confidential
 */

package com.vmware.blockchain.deployment.service.provision

import com.vmware.blockchain.deployment.v1.Endpoint
import com.vmware.blockchain.deployment.v1.OrchestrationSiteInfo

/**
 * This class helps in addition of default metadata in a given orchestration site.
 */
class OrchestrationSites {

    companion object {
        fun buildSiteInfo(
            originalSiteInfo: OrchestrationSiteInfo,
            containerRegistry: Endpoint,
            allocationServer: Endpoint
        ): OrchestrationSiteInfo {
            var original: OrchestrationSiteInfo = originalSiteInfo

            if (original.type == OrchestrationSiteInfo.Type.VSPHERE) {
                if (originalSiteInfo.vsphere.containerRegistry.address.isBlank()) {
                    original = original.copy(
                            vsphere = original.vsphere.copy(
                                    containerRegistry = containerRegistry
                            )
                    )
                }

                if (originalSiteInfo.vsphere.vsphere.network.allocationServer.address.isBlank()) {
                    original = original.copy(
                            vsphere = original.vsphere.copy(
                                    vsphere = original.vsphere.vsphere.copy(
                                            network = original.vsphere.vsphere.network.copy(
                                                    allocationServer = allocationServer
                                            )
                                    )
                            )
                    )
                }
            }
            if (original.type == OrchestrationSiteInfo.Type.VMC) {
                if (originalSiteInfo.vmc.containerRegistry.address.isBlank()) {
                    original = original.copy(
                            vmc = original.vmc.copy(
                                    containerRegistry = containerRegistry
                            )
                    )
                }

                if (originalSiteInfo.vmc.vsphere.network.allocationServer.address.isBlank()) {
                    original = original.copy(
                            vmc = original.vmc.copy(
                                    vsphere = original.vmc.vsphere.copy(
                                            network = original.vmc.vsphere.network.copy(
                                                    allocationServer = allocationServer
                                            )
                                    )
                            )
                    )
                }
            }
            return original
        }
    }
}
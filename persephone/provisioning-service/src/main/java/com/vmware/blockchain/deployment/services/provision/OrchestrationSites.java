/* **************************************************************************
 * Copyright (c) 2019 VMware, Inc. All rights reserved. VMware Confidential
 * **************************************************************************/

package com.vmware.blockchain.deployment.services.provision;

import org.apache.logging.log4j.util.Strings;

import com.vmware.blockchain.deployment.v1.Endpoint;
import com.vmware.blockchain.deployment.v1.OrchestrationSiteInfo;
import com.vmware.blockchain.deployment.v1.VSphereOrchestrationSiteInfo;
import com.vmware.blockchain.deployment.v1.VmcOrchestrationSiteInfo;

/**
 * Helper class for @OrchestrationSite.
 */

public class OrchestrationSites {

    /**
     * Helper util to override build site.
     * @param originalSiteInfo info
     * @param containerRegistry registry
     * @return info
     */
    public static final OrchestrationSiteInfo buildSiteInfo(OrchestrationSiteInfo originalSiteInfo,
                                                            Endpoint containerRegistry) {
        OrchestrationSiteInfo original = originalSiteInfo;
        if (originalSiteInfo.getType() == OrchestrationSiteInfo.Type.VSPHERE) {
            if (Strings.isEmpty(originalSiteInfo.getVsphere().getContainerRegistry().getAddress())) {
                original = OrchestrationSiteInfo.newBuilder(original)
                        .setVsphere(VSphereOrchestrationSiteInfo.newBuilder(original.getVsphere())
                                            .setContainerRegistry(containerRegistry).build()).build();
            }
        }

        if (original.getType() == OrchestrationSiteInfo.Type.VMC) {
            if (Strings.isEmpty(originalSiteInfo.getVmc().getContainerRegistry().getAddress())) {

                original = OrchestrationSiteInfo.newBuilder(original)
                        .setVmc(VmcOrchestrationSiteInfo.newBuilder(original.getVmc())
                                        .setContainerRegistry(containerRegistry).build()).build();
            }
        }

        return original;
    }
}
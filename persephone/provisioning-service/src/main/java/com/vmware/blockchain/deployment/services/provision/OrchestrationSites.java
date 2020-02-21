/* **************************************************************************
 * Copyright (c) 2019 VMware, Inc. All rights reserved. VMware Confidential
 * **************************************************************************/

package com.vmware.blockchain.deployment.services.provision;

import org.apache.logging.log4j.util.Strings;
import org.jetbrains.annotations.NotNull;

import com.vmware.blockchain.deployment.v1.Endpoint;
import com.vmware.blockchain.deployment.v1.IPv4Network;
import com.vmware.blockchain.deployment.v1.OrchestrationSiteInfo;
import com.vmware.blockchain.deployment.v1.VSphereDatacenterInfo;
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
     * @param allocationServer ipam
     * @return info
     */
    @NotNull
    public static final OrchestrationSiteInfo buildSiteInfo(@NotNull OrchestrationSiteInfo originalSiteInfo,
                                                            @NotNull Endpoint containerRegistry,
                                                            @NotNull Endpoint allocationServer) {
        OrchestrationSiteInfo original = originalSiteInfo;
        if (originalSiteInfo.getType() == OrchestrationSiteInfo.Type.VSPHERE) {
            if (Strings.isEmpty(originalSiteInfo.getVsphere().getContainerRegistry().getAddress())) {
                original = OrchestrationSiteInfo.newBuilder(original)
                        .setVsphere(VSphereOrchestrationSiteInfo.newBuilder(original.getVsphere())
                                            .setContainerRegistry(containerRegistry).build()).build();
            }

            if (Strings.isEmpty(
                    originalSiteInfo.getVsphere().getVsphere().getNetwork().getAllocationServer().getAddress())) {

                original = OrchestrationSiteInfo.newBuilder(original)
                        .setVsphere(VSphereOrchestrationSiteInfo.newBuilder(original.getVsphere())
                                            .setVsphere(
                                                    VSphereDatacenterInfo.newBuilder(original.getVsphere().getVsphere())
                                                            .setNetwork(IPv4Network.newBuilder(
                                                                    originalSiteInfo.getVsphere().getVsphere()
                                                                            .getNetwork())
                                                                                .setAllocationServer(allocationServer)
                                                                                .build()).build()).build()).build();
            }
        }

        if (original.getType() == OrchestrationSiteInfo.Type.VMC) {
            if (Strings.isEmpty(originalSiteInfo.getVmc().getContainerRegistry().getAddress())) {

                original = OrchestrationSiteInfo.newBuilder(original)
                        .setVmc(VmcOrchestrationSiteInfo.newBuilder(original.getVmc())
                                        .setContainerRegistry(containerRegistry).build()).build();
            }

            if (Strings
                    .isEmpty(originalSiteInfo.getVmc().getVsphere().getNetwork().getAllocationServer().getAddress())) {

                original = OrchestrationSiteInfo.newBuilder(original)
                        .setVmc(VmcOrchestrationSiteInfo.newBuilder(original.getVmc())
                                        .setVsphere(
                                                VSphereDatacenterInfo.newBuilder(original.getVmc().getVsphere())
                                                        .setNetwork(IPv4Network.newBuilder(
                                                                originalSiteInfo.getVmc().getVsphere()
                                                                        .getNetwork())
                                                                            .setAllocationServer(allocationServer)
                                                                            .build()).build()).build()).build();
            }
        }

        return original;
    }
}
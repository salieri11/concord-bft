/* **************************************************************************
 * Copyright (c) 2019 VMware, Inc. All rights reserved. VMware Confidential
 * **************************************************************************/

package com.vmware.blockchain.deployment.service.provision;

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
                original = new OrchestrationSiteInfo(original.getType(),
                                                     original.getLabels(),
                                                     new VmcOrchestrationSiteInfo(),
                                                     new VSphereOrchestrationSiteInfo(
                                                             original.getVsphere().getApi(),
                                                             containerRegistry,
                                                             original.getVsphere().getVsphere(),
                                                             original.getVsphere().getLogManagements()));
            }

            if (Strings.isEmpty(
                    originalSiteInfo.getVsphere().getVsphere().getNetwork().getAllocationServer().getAddress())) {
                original = new OrchestrationSiteInfo(original.getType(),
                                                     original.getLabels(),
                                                     new VmcOrchestrationSiteInfo(),
                                                     new VSphereOrchestrationSiteInfo(
                                                             original.getVsphere().getApi(),
                                                             containerRegistry,
                                                             new VSphereDatacenterInfo(
                                                                     original.getVsphere().getVsphere()
                                                                             .getResourcePool(),
                                                                     original.getVsphere().getVsphere().getDatastore(),
                                                                     original.getVsphere().getVsphere().getFolder(),
                                                                     new IPv4Network(original.getVsphere().getVsphere()
                                                                                             .getNetwork().getName(),
                                                                                     original.getVsphere().getVsphere()
                                                                                             .getNetwork()
                                                                                             .getAddressAllocation(),
                                                                                     original.getVsphere().getVsphere()
                                                                                             .getNetwork().getGateway(),
                                                                                     original.getVsphere().getVsphere()
                                                                                             .getNetwork().getSubnet(),
                                                                                     allocationServer,
                                                                                     original.getVsphere().getVsphere()
                                                                                             .getNetwork()
                                                                                             .getNameServers()),
                                                                     original.getVsphere().getVsphere()
                                                                             .getOutboundProxy()),
                                                             original.getVsphere().getLogManagements()));
            }
        }

        if (original.getType() == OrchestrationSiteInfo.Type.VMC) {
            if (Strings.isEmpty(originalSiteInfo.getVmc().getContainerRegistry().getAddress())) {
                original = new OrchestrationSiteInfo(
                        original.getType(),
                        original.getLabels(),
                        new VmcOrchestrationSiteInfo(original.getVmc().getAuthentication(),
                                                     original.getVmc().getApi(),
                                                     containerRegistry,
                                                     original.getVmc().getOrganization(),
                                                     original.getVmc().getDatacenter(),
                                                     original.getVmc().getVsphere(),
                                                     original.getVmc()
                                                             .getLogManagements()),
                        new VSphereOrchestrationSiteInfo());
            }

            if (Strings
                    .isEmpty(originalSiteInfo.getVmc().getVsphere().getNetwork().getAllocationServer().getAddress())) {
                original = new OrchestrationSiteInfo(
                        original.getType(),
                        original.getLabels(),
                        new VmcOrchestrationSiteInfo(original.getVmc().getAuthentication(),
                                                     original.getVmc().getApi(),
                                                     containerRegistry,
                                                     original.getVmc().getOrganization(),
                                                     original.getVmc().getDatacenter(),

                                                     new VSphereDatacenterInfo(
                                                             original.getVmc().getVsphere()
                                                                     .getResourcePool(),
                                                             original.getVmc().getVsphere()
                                                                     .getDatastore(),
                                                             original.getVmc().getVsphere()
                                                                     .getFolder(),
                                                             new IPv4Network(
                                                                     original.getVmc()
                                                                             .getVsphere()
                                                                             .getNetwork()
                                                                             .getName(),
                                                                     original.getVmc()
                                                                             .getVsphere()
                                                                             .getNetwork()
                                                                             .getAddressAllocation(),
                                                                     original.getVmc()
                                                                             .getVsphere()
                                                                             .getNetwork()
                                                                             .getGateway(),
                                                                     original.getVmc()
                                                                             .getVsphere()
                                                                             .getNetwork()
                                                                             .getSubnet(),
                                                                     allocationServer,
                                                                     original.getVmc()
                                                                             .getVsphere()
                                                                             .getNetwork()
                                                                             .getNameServers()),
                                                             original.getVmc().getVsphere()
                                                                     .getOutboundProxy()),
                                                     original.getVmc()
                                                             .getLogManagements()),
                        new VSphereOrchestrationSiteInfo());
            }
        }

        return original;
    }
}
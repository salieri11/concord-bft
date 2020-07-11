/*
 * Copyright (c) 2020 VMware, Inc. All rights reserved. VMware Confidential
 */

package com.vmware.blockchain.deployment.services.orchestration.vmware;

import com.vmware.blockchain.deployment.services.orchestration.Orchestrator;
import com.vmware.blockchain.deployment.services.orchestration.OrchestratorProvider;
import com.vmware.blockchain.deployment.services.orchestration.inactive.InactiveOrchestrator;
import com.vmware.blockchain.deployment.services.orchestration.ipam.IpamClient;
import com.vmware.blockchain.deployment.services.orchestration.vmc.VmcOrchestrator;
import com.vmware.blockchain.deployment.services.orchestration.vsphere.VSphereOrchestrator;
import com.vmware.blockchain.deployment.v1.OrchestrationSiteInfo;

/**
 * A concrete implementation of [OrchestratorProvider].
 */
public class OrchestratorFactory  implements OrchestratorProvider {

    /**
     * Create a new [Orchestrator] based on the input [OrchestrationSiteInfo].
     *
     * @param[site]
     *   information pertaining to the orchestration site to create orchestrator driver for.
     *
     * @return
     *   a new concrete implementation of an [Orchestrator] instance for the input site type if
     *   supported, [InactiveOrchestrator] instance otherwise.
     */
    public Orchestrator newOrchestrator(OrchestrationSiteInfo site, IpamClient ipamClient) {
        if (site.getType() == OrchestrationSiteInfo.Type.VMC) {
            return newVmcOrchestrator(site, ipamClient);
        } else if (site.getType() == OrchestrationSiteInfo.Type.VSPHERE) {
            return newVSphereOrchestrator(site, ipamClient);
        } else {
            return new InactiveOrchestrator(site);
        }
    }

    /**
     * Create a new [VSphereOrchestrator] based on parameters from a given
     * [OrchestrationSiteInfo].
     *
     * @param[site]
     *   orchestration information pertaining to a VMC-based orchestration site.
     *
     * @return
     *   a [VSphereOrchestrator] instance corresponding to the given input parameter.
     */
    private Orchestrator newVSphereOrchestrator(OrchestrationSiteInfo site, IpamClient ipamClient) {
        return new VSphereOrchestrator(site.getVsphere().getVsphere(), site.getVsphere().getApi(), ipamClient);
    }

    /**
     * Create a new [VmcOrchestrator] based on parameters from a given [OrchestrationSiteInfo].
     *
     * @param[site]
     *   orchestration information pertaining to a VMC-based orchestration site.
     *
     * @return
     *   a [VmcOrchestrator] instance corresponding to the given input parameter.
     */
    private VmcOrchestrator newVmcOrchestrator(OrchestrationSiteInfo site, IpamClient ipamClient) {
        return new VmcOrchestrator(site.getVmc(), ipamClient);
    }
}

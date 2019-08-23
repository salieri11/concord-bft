/* **************************************************************************
 * Copyright (c) 2019 VMware, Inc. All rights reserved. VMware Confidential
 * **************************************************************************/
package com.vmware.blockchain.deployment.orchestration.vmware

import com.vmware.blockchain.deployment.model.core.URI
import com.vmware.blockchain.deployment.orchestration.InactiveOrchestrator
import com.vmware.blockchain.deployment.orchestration.Orchestrator
import com.vmware.blockchain.deployment.orchestration.OrchestratorProvider
import com.vmware.blockchain.deployment.v1.OrchestrationSiteInfo
import com.vmware.blockchain.deployment.vmc.VmcClient
import com.vmware.blockchain.deployment.vmc.VmcHttpClient
import com.vmware.blockchain.deployment.vmc.VmcModelSerializer
import com.vmware.blockchain.deployment.vmc.VmcOrchestrator
import com.vmware.blockchain.deployment.vsphere.VSphereClient
import com.vmware.blockchain.deployment.vsphere.VSphereHttpClient
import com.vmware.blockchain.deployment.vsphere.VSphereModelSerializer
import com.vmware.blockchain.deployment.vsphere.VSphereOrchestrator
import kotlin.coroutines.CoroutineContext
import kotlinx.coroutines.CoroutineScope
import kotlinx.coroutines.Dispatchers

/**
 * A concrete implementation of [OrchestratorProvider].
 */
object OrchestratorFactory : OrchestratorProvider, CoroutineScope {

    /** [CoroutineContext] to launch all coroutines associated with this singleton object. */
    override val coroutineContext: CoroutineContext
        get() = Dispatchers.Default

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
    override fun newOrchestrator(site: OrchestrationSiteInfo): Orchestrator {
        return when (site.type) {
                OrchestrationSiteInfo.Type.VMC -> newVmcOrchestrator(site)
                OrchestrationSiteInfo.Type.VSPHERE -> newVSphereOrchestrator(site)
                else -> InactiveOrchestrator(site)
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
    private fun newVSphereOrchestrator(site: OrchestrationSiteInfo): VSphereOrchestrator {
        // Precondition.
        require(site.type == OrchestrationSiteInfo.Type.VSPHERE)
        val info = requireNotNull(site.vsphere)

        // Create new vSphere client.
        val context = VSphereHttpClient.Context(
                endpoint = URI.create(info.api.address),
                username = info.api.credential.passwordCredential.username,
                password = info.api.credential.passwordCredential.password
        )
        val vSphereClient = VSphereClient(
                VSphereHttpClient(context, VSphereModelSerializer, allowInsecureConnection = true)
        )

        return VSphereOrchestrator(info, vSphereClient)
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
    private fun newVmcOrchestrator(site: OrchestrationSiteInfo): VmcOrchestrator {
        // Precondition.
        require(site.type == OrchestrationSiteInfo.Type.VMC)

        val info = requireNotNull(site.vmc)
        val token = requireNotNull(info.authentication.credential.tokenCredential).token

        // Create new VMC client.
        val vmcContext = VmcHttpClient.Context(
                endpoint = URI.create(info.api.address),
                authenticationEndpoint = URI.create(info.authentication.address),
                refreshToken = token,
                organization = info.organization,
                datacenter = info.datacenter,
                enableVerboseLogging = false
        )
        val vmcClient = VmcClient(VmcHttpClient(vmcContext, VmcModelSerializer))

        return VmcOrchestrator(info, vmcClient)
    }
}

/* **************************************************************************
 * Copyright (c) 2019 VMware, Inc.  All rights reserved. VMware Confidential
 * *************************************************************************/
package com.vmware.blockchain.deployment.service.provision

import com.vmware.blockchain.deployment.orchestration.Orchestrator
import com.vmware.blockchain.deployment.v1.OrchestrationSiteIdentifier

/**
 * Context encapsulating the runtime parameters and states of a running deployment session.
 */
data class DeploymentSessionContext(
    val orchestrators: Map<OrchestrationSiteIdentifier, Orchestrator>
)

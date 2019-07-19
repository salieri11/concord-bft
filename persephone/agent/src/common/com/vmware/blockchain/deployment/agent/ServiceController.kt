/* **************************************************************************
 * Copyright (c) 2019 VMware, Inc.  All rights reserved. VMware Confidential
 * *************************************************************************/
package com.vmware.blockchain.deployment.agent

import com.vmware.blockchain.deployment.agent.docker.DockerOrchestrator
import com.vmware.blockchain.deployment.model.ConcordComponent

class ServiceController(
    private val orchestrator: DockerOrchestrator,
    private val components: List<ConcordComponent>
) {
    // PLACE-HOLDER.
}

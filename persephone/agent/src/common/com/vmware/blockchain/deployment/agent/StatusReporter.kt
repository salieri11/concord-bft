/* **************************************************************************
 * Copyright (c) 2019 VMware, Inc.  All rights reserved. VMware Confidential
 * *************************************************************************/
package com.vmware.blockchain.deployment.agent

import com.vmware.blockchain.deployment.v1.InstanceMessage

/**
 * Reporter for all deployed Concord components on a given Concord node instance.
 */
class StatusReporter {

    /**
     * Obtain the current status.
     *
     * @return
     *   current status summarized in a [InstanceMessage.Status] instance.
     */
    fun currentStatus(): InstanceMessage.Status {
        return InstanceMessage.Status(
                state = InstanceMessage.Status.State.ACTIVE
        )
    }
}

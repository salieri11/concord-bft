/* **************************************************************************
 * Copyright (c) 2019 VMware, Inc.  All rights reserved. VMware Confidential
 * *************************************************************************/
package com.vmware.blockchain.deployment.model.vmc

import kotlinx.serialization.Serializable

@Serializable
data class Sddc(val resource_config: SddcResourceConfig)

/* **************************************************************************
 * Copyright (c) 2019 VMware, Inc.  All rights reserved. VMware Confidential
 * *************************************************************************/
package com.vmware.blockchain.deployment.model.vsphere

import kotlinx.serialization.ContextualSerialization
import kotlinx.serialization.Serializable

/**
 * Simplified binding of OVF property parameter.
 */
@Serializable
data class OvfProperty(val id: String,
                       @ContextualSerialization
                       val value: Any)

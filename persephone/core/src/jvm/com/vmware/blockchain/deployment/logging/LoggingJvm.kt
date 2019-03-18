/* **************************************************************************
 * Copyright (c) 2019 VMware, Inc.  All rights reserved. VMware Confidential
 * *************************************************************************/
package com.vmware.blockchain.deployment.logging

/**
 * Type alias redeclaration of [org.slf4j.Logger] to syntactically shield logging consumers.
 */
actual typealias Logger = org.slf4j.Logger

/**
 * Create or retrieve a [Logger] by the supplied name.
 *
 * @param[name]
 *   name of the logger.
 *
 * @return
 *   the [Logger] instance corresponding to the supplied name.
 */
actual fun getLogger(name: String): Logger = org.slf4j.LoggerFactory.getLogger(name)

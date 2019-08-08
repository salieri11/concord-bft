/* **************************************************************************
 * Copyright (c) 2019 VMware, Inc.  All rights reserved. VMware Confidential
 * *************************************************************************/
package com.vmware.blockchain.deployment.http

import java.security.cert.X509Certificate
import javax.net.ssl.X509TrustManager

/**
 * An implementation of [X509TrustManager] that trusts all certificates.
 */
object InsecureX509TrustManager : X509TrustManager {
    override fun getAcceptedIssuers(): Array<X509Certificate> = emptyArray()

    override fun checkClientTrusted(certs: Array<X509Certificate>, authType: String) { }

    override fun checkServerTrusted(certs: Array<X509Certificate>, authType: String) { }
}

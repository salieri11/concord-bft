/* **********************************************************************
 * Copyright 2018 VMware, Inc.  All rights reserved. VMware Confidential
 * *********************************************************************/
package com.vmware.blockchain.deployment.http

/**
 * Contract of an HTTP response containing a typed response body.
 *
 * @param[T]
 *   type of the response body.
 */
interface HttpResponse<T> {
    /**
     * The status code of this HTTP response.
     *
     * @return
     *   the status code as an [Int].
     */
    fun statusCode(): Int

    /**
     * The body of this HTTP response.
     *
     * @return
     *   the body as an instance of type [T].
     */
    fun body(): T
}

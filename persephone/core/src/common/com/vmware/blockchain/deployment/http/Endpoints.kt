/* **************************************************************************
 * Copyright (c) 2019 VMware, Inc.  All rights reserved. VMware Confidential
 * *************************************************************************/
package com.vmware.blockchain.deployment.http

import com.vmware.blockchain.deployment.model.core.URI

/**
 * Denote an enumeration of HTTP endpoints at a given base service URI.
 *
 * Note: This interface is designed to be implemented by enum classes where each enum instance
 * represents a service endpoint.
 *
 * @property[path]
 *   endpoint path.
 * @property[parameterMappings]
 *   set of possible parameters that can be specified for the endpoint.
 * @property[pathMappings]
 *   set of possible path parameters that must be specified for the endpoint.
 */
interface EndpointEnumeration {

    val path: String
    val parameterMappings: Set<String>
    val pathMappings: Set<String>

    /**
     * Interpolate the expected URL by associating the given set of parameters with values.
     *
     * @param[parameters]
     *   list of pairs of parameter name and values.
     * @param[pathVariables]
     *   list of pairs of path variable name and values.
     *
     * @return
     *   the expected URL, as a [String].
     */
    fun interpolate(
        parameters: List<Pair<String, String>> = emptyList(),
        pathVariables: List<Pair<String, String>> = emptyList()
    ): String {
        val unresolved = path
        val resolved = pathVariables
                .filter { pathMappings.contains(it.first) }
                .fold(unresolved) {
                    current, mapping -> current.replace(mapping.first, mapping.second)
                }
        return parameters
                .filter { parameterMappings.contains(it.first) }
                .takeIf { it.isNotEmpty() }
                ?.joinToString(prefix = "$resolved?", separator = "&",
                               transform = { (key, value) -> "$key=$value" })
                ?: resolved
    }

    /**
     * Resolve the endpoint.
     *
     * @param[base]
     *   base URI of the endpoint.
     * @param[parameters]
     *   list of pairs of parameter name and values.
     * @param[pathVariables]
     *   list of pairs of path variable name and values.
     *
     * @return
     *   a resolved URL, as a [URI].
     */
    fun resolve(
        base: URI,
        parameters: List<Pair<String, String>> = emptyList(),
        pathVariables: List<Pair<String, String>> = emptyList()
    ): URI {
        return base.resolve(interpolate(parameters, pathVariables))
    }
}

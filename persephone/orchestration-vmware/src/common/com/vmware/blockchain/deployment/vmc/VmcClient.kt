/* **************************************************************************
 * Copyright (c) 2019 VMware, Inc.  All rights reserved. VMware Confidential
 * *************************************************************************/
package com.vmware.blockchain.deployment.vmc

import com.vmware.blockchain.deployment.logging.logger
import com.vmware.blockchain.deployment.model.core.URI
import com.vmware.blockchain.deployment.model.nsx.NatRule
import com.vmware.blockchain.deployment.model.nsx.PublicIP
import com.vmware.blockchain.deployment.model.nsx.Segment
import com.vmware.blockchain.deployment.model.nsx.SegmentSubnet
import com.vmware.blockchain.deployment.model.vmc.Sddc
import com.vmware.blockchain.deployment.orchestration.randomSubnet
import com.vmware.blockchain.deployment.orchestration.toIPv4Address
import kotlinx.coroutines.delay
import kotlin.math.pow

/**
 * A client for issuing commands to a VMware Cloud on AWS environment targeted by a given
 * [VmcHttpClient].
 *
 * @property[client]
 *   underlying [VmcHttpClient] to use for REST API communication.
 */
class VmcClient(private val client: VmcHttpClient) {

    /** Logging instance. */
    private val log by logger()

    /**
     * Get SDDC information associated with the associated [VmcHttpClient].
     *
     * @return
     *   information pertaining to SDDC, as a [Sddc] instance.
     */
    suspend fun getDataCenterInfo(): Sddc? {
        return client
                .get<Sddc>(
                        Endpoints.VMC_SDDC
                                .interpolate(pathVariables = listOf(
                                        "{org}" to client.context.organization,
                                        "{sddc}" to client.context.datacenter)
                                ),
                        contentType = "application/json",
                        headers = emptyList()
                )
                .takeIf { it.statusCode() == 200 }
                ?.let { it.body() }
    }

    /**
     * Allocate a new public IP address for use, with a given name as the identifier.
     *
     * @param[name]
     *   identifier of the IP address resource.
     *
     * @return
     *   allocated IP address resource as a instance of [PublicIP], if created, `null` otherwise.
     */
    suspend fun createPublicIP(name: String): PublicIP? {
        return client
                .put<PublicIP, PublicIP>(
                        path = Endpoints.VMC_PUBLIC_IP
                                .interpolate(pathVariables = listOf("{ip_id}" to name)),
                        contentType = "application/json",
                        headers = emptyList(),
                        body = PublicIP(id = name, display_name = name)
                )
                .takeIf { it.statusCode() == 200 }
                ?.body()
    }

    /**
     * Retrieve the public IP resource with a given resource identifier.
     *
     * @param[id]
     *   identifier of the IP address resource.
     *
     * @return
     *   IP address resource as a instance of [PublicIP] if found, `null` otherwise.
     */
    suspend fun getPublicIP(id: String): PublicIP? {
        return client
                .get<PublicIP>(
                        resolvePublicIPIdentifier(id).toString(),
                        contentType = "application/json",
                        headers = emptyList()
                )
                .takeIf { it.statusCode() == 200 }
                ?.body()
    }

    /**
     * Create a NAT rule based on the given parameters.
     *
     * @param[tier1]
     *   tier-1 network that the NAT region is under for NAT rule addition.
     * @param[nat]
     *   NAT region to add rule to.
     * @param[name]
     *   identifier to assign to the NAT rule.
     * @param[action]
     *   type of NAT action.
     * @param[sourceNetwork]
     *   source address(es) to translate for SNAT and REFLEXIVE rules.
     * @param[destinationNetwork]
     *   destination address(es) to translate for DNAT rules.
     * @param[translatedNetwork]
     *   network address to apply translation into.
     * @param[translatedPorts]
     *   network ports to apply translation.
     */
    suspend fun createNatRule(
        tier1: String = "cgw",
        nat: String = "USER",
        name: String,
        action: NatRule.Action,
        sourceNetwork: String? = null,
        destinationNetwork: String? = null,
        translatedNetwork: String? = null,
        translatedPorts: String? = null,
        maxRetries : Int = 3,
        maxDelayInSeconds : Long = 20L
    ): NatRule? {
        val endpoint = Endpoints.NSX_NAT_RULE.interpolate(
                pathVariables = listOf("{tier1}" to tier1, "{nat}" to nat, "{nat_rule}" to name)
        )

        for(retries in 1..maxRetries) {

            log.info("Creating NAT rule - attempt $retries")
            val response = client
                    .patch<NatRule, Unit>(
                            path = endpoint,
                            contentType = "application/json",
                            headers = emptyList(),
                            body = NatRule(
                                    action = action,
                                    source_network = sourceNetwork,
                                    destination_network = destinationNetwork,
                                    translated_network = translatedNetwork,
                                    translated_ports = translatedPorts
                            )
                    )

            // PATCH does not return a body, follow up with another GET to obtain metadata information
            // that is assigned post-creation (e.g. ID, infra path, etc).
            when (response.statusCode()) {
                200 -> {
                    return client.get<NatRule>(path = endpoint,
                                        contentType = "application/json",
                                        headers = emptyList())
                                        .takeIf { it.statusCode() == 200 }?.body()
                }
                500 -> {
                    val delay = 2.0.pow(retries + 1).toLong().coerceAtMost(maxDelayInSeconds)
                    if (retries < maxRetries) // do not delay after last retry
                        delay(delay * 1000L)
                }
                else -> return null
            }

        }

        return null

    }

    /**
     * Get information regarding a specified tier-1 network segment based on name.
     *
     * @param[tier1]
     *   identifier of the tier-1 network.
     * @param[name]
     *   name of the network segment to look up.
     *
     * @return
     *   information regarding the network as a [Segment], if found.
     */
    private suspend fun getNetworkSegment(tier1: String = "cgw", name: String): Segment? {
        return client
                .get<Segment>(
                        Endpoints.NSX_NETWORK_SEGMENT
                                .interpolate(pathVariables = listOf("{tier1}" to tier1,
                                                                    "{segment}" to name)),
                        contentType = "application/json",
                        headers = emptyList()
                )
                .takeIf { it.statusCode() == 200 }
                ?.let { it.body() }
    }

    /**
     * Create a routed NSX logical network segment with the given parameters.
     *
     * @param[tier1]
     *   tier-1 network to create the logical network segment under.
     * @param[name]
     *   name of the network segment to create.
     * @param[prefix]
     *   network prefix range to create the network segment under.
     * @param[prefixSubnet]
     *   subnet size for the network prefix range.
     * @param[subnetSize]
     *   subnet size for the network segment itself.
     *
     * @return
     *   ID of the network segment as a [String], if created.
     */
    suspend fun createNetworkSegment(
        tier1: String = "cgw",
        name: String,
        prefix: Int,
        prefixSubnet: Int,
        subnetSize: Int = 28
    ): String? {
        // Generate a network model based on a randomly generated subnet.
        // Note (current implementation choices):
        // - Primary address is set to the first address in the subnet.
        // - DHCP pool can assign addresses from second address to subnet-broadcast (max) - 1.
        val subnet = randomSubnet(prefix, prefixSubnet, subnetSize)
        val subnetMax = subnet + (1 shl (Int.SIZE_BITS - subnetSize)) - 1
        val segmentSubnet = SegmentSubnet(
                gateway_address = "${(subnet + 1).toIPv4Address()}/$subnetSize",
                dhcp_ranges = listOf(
                        "${(subnet + 2).toIPv4Address()}-${(subnetMax - 1).toIPv4Address()}"
                )
        )
        val segment = Segment(subnets = listOf(segmentSubnet))

        val response = client
                .patch<Segment, Segment>(
                        Endpoints.NSX_NETWORK_SEGMENT
                                .interpolate(pathVariables = listOf("{tier1}" to tier1,
                                                                    "{segment}" to name)),
                        contentType = "application/json",
                        headers = emptyList(),
                        body = segment
                )

        return when (response.statusCode()) {
            200 -> name
            else -> null
        }
    }

    /**
     * Delete the VMware Cloud resource referenced by the given URI.
     *
     * @param[resource]
     *   resource URI.
     *
     * @return
     *   `true` if deleted, `false` otherwise.
     */
    suspend fun deleteResource(resource: URI): Boolean {
        val response = client.delete<Unit>(
                resource.toString(),
                contentType = "application/json",
                headers = emptyList()
        )

        return when (response.statusCode()) {
            200 -> true
            else -> false
        }
    }

    /**
     * Resolve a public IP identifier to its canonical resource URI as perceived by the target
     * VMware Cloud server endpoint.
     *
     * @param[id]
     *   public IP identifier.
     *
     * @return
     *   public IP resource URL as an instance of [URI].
     */
    fun resolvePublicIPIdentifier(id: String): URI {
        return Endpoints.VMC_PUBLIC_IP
                .resolve(client.context.endpoint, pathVariables = listOf("{ip_id}" to id))
    }

    /**
     * Resolve a NSX Policy resource identifier to its canonical resource URI as perceived by the
     * target VMware Cloud server endpoint.
     *
     * @param[resource]
     *   NSX policy resource identifier.
     *
     * @return
     *   NSX Policy resource URL as an instance of [URI].
     */
    fun resolveNsxResourceIdentifier(resource: String): URI {
        return Endpoints.NSX_API_ROOT
                .resolve(client.context.endpoint, pathVariables = listOf("{resource}" to resource))
    }
}

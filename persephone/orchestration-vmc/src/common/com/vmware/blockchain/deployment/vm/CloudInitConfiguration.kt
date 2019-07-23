/* **************************************************************************
 * Copyright (c) 2019 VMware, Inc.  All rights reserved. VMware Confidential
 * *************************************************************************/
package com.vmware.blockchain.deployment.vm

import com.vmware.blockchain.deployment.http.JsonSerializer
import com.vmware.blockchain.deployment.model.ConcordAgentConfiguration
import com.vmware.blockchain.deployment.model.ConcordClusterIdentifier
import com.vmware.blockchain.deployment.model.ConcordComponent
import com.vmware.blockchain.deployment.model.ConcordModelSpecification
import com.vmware.blockchain.deployment.model.ConcordNodeIdentifier
import com.vmware.blockchain.deployment.model.ConfigurationSessionIdentifier
import com.vmware.blockchain.deployment.model.Credential
import com.vmware.blockchain.deployment.model.Endpoint
import com.vmware.blockchain.deployment.model.core.URI
import com.vmware.blockchain.deployment.model.ethereum.Genesis
import com.vmware.blockchain.deployment.orchestration.toIPv4Address
import com.vmware.blockchain.deployment.orchestration.toIPv4SubnetMask
import kotlinx.serialization.modules.serializersModuleOf

/**
 * Initialization script run either on first-boot of a deployed virtual machine.
 */
class CloudInitConfiguration(
    containerRegistry: Endpoint,
    model: ConcordModelSpecification,
    genesis: Genesis,
    ipAddress: String,
    gateway: String,
    subnet: Int,
    clusterId: ConcordClusterIdentifier,
    nodeId: ConcordNodeIdentifier,
    configGenId: ConfigurationSessionIdentifier
) {
    object GenesisSerializer
        : JsonSerializer(serializersModuleOf(Genesis::class, Genesis.serializer()))

    object ConcordAgentConfigurationSerializer
        : JsonSerializer(
            serializersModuleOf(
                    ConcordAgentConfiguration::class, ConcordAgentConfiguration.serializer()
            )
    )

    /** Consolidated Docker PULL command. */
    private val dockerPullCommand: String = model.components.asSequence()
            .filter { it.type == ConcordComponent.Type.CONTAINER_IMAGE }
            .map { "docker pull ${URI.create(containerRegistry.address).authority}/${it.name}" }
            .joinToString(separator = "\n", postfix = "\n")

    /** Network configuration command. */
    private val networkAddressCommand: String by lazy {
        val netmask = toIPv4SubnetMask(subnet).toIPv4Address()
        val nic = "`basename -a /sys/class/net/* | grep -e eth -e ens | sort -u | head -1`"
        ipAddress.takeIf { it.isNotBlank() }
                ?.let {
                    "ifconfig $nic $ipAddress netmask $netmask && route add default gw $gateway $nic"
                }
                // ?.let { "tdnf install netmgmt -y && netmgr ip4_address --set --interface eth0 --mode static --addr $ipAddress/$subnet --gateway $gateway" }
                ?: "" // No-action defaults to DHCP.
    }

    /** Concord agent startup configuration parameters. */
    private val configuration: ConcordAgentConfiguration = ConcordAgentConfiguration (
            model = model,
            containerRegistry = containerRegistry,
            fleetService = Endpoint(), // TODO: need to inject fleet service endpoint info.
            cluster = clusterId,
            node = nodeId,
            configServiceEndpoint = Endpoint(), // FIXME: need to inject config service endpoint.
            configurationSession =  configGenId

    )

    private val script =
            """
            #!/bin/sh
            echo -e "c0nc0rd\nc0nc0rd" | /bin/passwd

            # Configure network
            mkdir -p /concord/agent/network
            echo '{{staticIp}}' > /config/network/ipaddr
            echo '{{gateway}}' > /config/network/gateway

            {{networkAddressCommand}}

            # Enable when there are multiple interfaces for separate networks.
            # route add default gw `ip route show | grep "dev eth0" | grep -v kernel | grep -v default | cut -d' ' -f 1` eth0

            sed -i 's_/usr/bin/dockerd_/usr/bin/dockerd -H tcp://127.0.0.1:2375 -H unix:///var/run/docker.sock_g' /lib/systemd/system/docker.service
            systemctl daemon-reload
            systemctl restart docker
            systemctl enable docker
            {{dockerLoginCommand}}
            {{dockerPullCommand}}

            # Output the node's configuration.
            mkdir -p /config/concord/config-local
            mkdir -p /config/concord/config-public

            # create dir for tls certs
            mkdir -p /concord/config-local/cert

            # Output the node's model specification.
            mkdir -p /config/agent
            echo '{{agentConfig}}' > /config/agent/config.json

            # Update guest-info's network information in vSphere.
            touch /etc/vmware-tools/tools.conf
            printf '[guestinfo]\nprimary-nics=eth*\nexclude-nics=docker*,veth*' > /etc/vmware-tools/tools.conf
            /usr/bin/vmware-toolbox-cmd info update network

            touch /config/concord/config-local/concord.config
            touch /config/concord/config-public/find-docker-instances.sh
            chmod 777 /config/concord/config-public/find-docker-instances.sh

            echo '{{genesis}}' > /config/concord/config-public/genesis.json
            docker run -d --name=agent --restart=always -v /config/agent/config.json:/config/config.json -v /config:/config -v /var/run/docker.sock:/var/run/docker.sock -p 8546:8546 registry-1.docker.io/vmwblockchain/agent-testing:debug
            echo 'done'
            """.trimIndent()
                    .replace("{{dockerLoginCommand}}", containerRegistry.toRegistryLoginCommand())
                    .replace("{{dockerPullCommand}}", dockerPullCommand)
                    .replace("{{genesis}}", GenesisSerializer.toJson(genesis))
                    .replace("{{agentConfig}}", ConcordAgentConfigurationSerializer.toJson(configuration))
                    .replace("{{networkAddressCommand}}", ipAddress.takeIf { it.isNotBlank() }?.let { networkAddressCommand }?: "")
                    .replace("{{staticIp}}", ipAddress)
                    .replace("{{gateway}}", gateway)

    /**
     * Convert an endpoint to a Docker Registry login command.
     *
     * @return
     *   docker login command as a [String].
     */
    private fun Endpoint.toRegistryLoginCommand(): String {
        val credential = when (credential.type) {
            Credential.Type.PASSWORD -> {
                val passwordCredential = requireNotNull(credential.passwordCredential)

                "-u ${passwordCredential.username} -p '${passwordCredential.password}'"
            }
            else -> ""
        }

        return "docker login $address $credential"
    }

    /**
     * Retrieve the user-data manifest of the Cloud-Init configuration.
     *
     * @return
     *   user-data manifest as a [String].
     */
    fun userData(): String = script
}

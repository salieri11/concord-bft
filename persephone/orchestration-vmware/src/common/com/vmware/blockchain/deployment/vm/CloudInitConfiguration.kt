/* **************************************************************************
 * Copyright (c) 2019 VMware, Inc.  All rights reserved. VMware Confidential
 * *************************************************************************/
package com.vmware.blockchain.deployment.vm

import com.vmware.blockchain.deployment.model.core.URI
import com.vmware.blockchain.deployment.orchestration.toIPv4Address
import com.vmware.blockchain.deployment.orchestration.toIPv4SubnetMask
import com.vmware.blockchain.deployment.http.JsonSerializer
import com.vmware.blockchain.deployment.v1.ConcordAgentConfiguration
import com.vmware.blockchain.deployment.v1.ConcordClusterIdentifier
import com.vmware.blockchain.deployment.v1.ConcordComponent
import com.vmware.blockchain.deployment.v1.ConcordModelSpecification
import com.vmware.blockchain.deployment.v1.ConfigurationSessionIdentifier
import com.vmware.blockchain.deployment.v1.Credential
import com.vmware.blockchain.deployment.v1.Endpoint
import com.vmware.blockchain.deployment.v1.OutboundProxyInfo
import com.vmware.blockchain.ethereum.type.Genesis
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
    nameServers: List<String>,
    subnet: Int,
    clusterId: ConcordClusterIdentifier,
    nodeId: Int,
    configGenId: ConfigurationSessionIdentifier,
    configServiceEndpoint: Endpoint,
    configServiceRestEndpoint: Endpoint,
    outboundProxy: OutboundProxyInfo
) {
    object GenesisSerializer
        : JsonSerializer(serializersModuleOf(Genesis::class, Genesis.serializer()))

    object ConcordAgentConfigurationSerializer
        : JsonSerializer(
            serializersModuleOf(
                    ConcordAgentConfiguration::class, ConcordAgentConfiguration.serializer()
            )
    )

    private val agentImageName: String = model.components.asSequence()
            .filter { it.type == ConcordComponent.Type.CONTAINER_IMAGE }
            .filter { it.serviceType == ConcordComponent.ServiceType.GENERIC }.first().name

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

    private val dnsSetupCommand: String by lazy {
        nameServers.takeIf { !it.isNullOrEmpty() }
                ?.let {
                    val dnsString = nameServers.joinToString(separator = " ")
                    "echo -e \"[Resolve]\\nDNS=$dnsString\" > /etc/systemd/resolved.conf ;" +
                            " systemctl restart systemd-resolved.service"
                }
                ?: ""
    }

    private val dockerDnsSetupCommand: String by lazy {
        nameServers.takeIf { !it.isNullOrEmpty() }
                ?.let {
                    nameServers.joinToString(separator = " --dns ",
                            prefix = "--dns ")
                }
                ?: ""
    }

    private val setupOutboundProxy: String by lazy {
        outboundProxy.takeIf { !outboundProxy.httpsHost.isNullOrBlank() }
                ?.let {
                    "export http_proxy=${outboundProxy.httpHost}:${outboundProxy.httpPort}" +
                            ";export https_proxy=${outboundProxy.httpsHost}:${outboundProxy.httpsPort}" +
                            ";mkdir /etc/systemd/system/docker.service.d" +
                            ";echo -e '[Service]\\nEnvironment=\"HTTP_PROXY=http://${outboundProxy.httpHost}:${outboundProxy.httpPort}\"\\nEnvironment=\"HTTPS_PROXY=https://${outboundProxy.httpsHost}:${outboundProxy.httpsPort}\"\\nEnvironment=\"NO_PROXY=localhost,127.0.0.1\"' > /etc/systemd/system/docker.service.d/http-proxy.conf"
                }
                ?: ""
    }

    /** Concord agent startup configuration parameters. */
    private val configuration: ConcordAgentConfiguration = ConcordAgentConfiguration (
            model = model,
            containerRegistry = containerRegistry,
            fleetService = Endpoint(), // TODO: need to inject fleet service endpoint info.
            cluster = clusterId,
            node = nodeId,
            configService = outboundProxy.takeIf { outboundProxy.httpsHost.isNullOrBlank() }
                    ?.let {
                        configServiceEndpoint
                    }
                    ?: configServiceRestEndpoint,
            configurationSession =  configGenId,
            outboundProxyInfo = outboundProxy
    )

    private val script =
            """
            #!/bin/sh
            echo -e "c0nc0rd\nc0nc0rd" | /bin/passwd
            {{networkAddressCommand}}
            {{dnsSetupCommand}}
            
            # Enable when there are multiple interfaces for separate networks.
            # route add default gw `ip route show | grep "dev eth0" | grep -v kernel | grep -v default | cut -d' ' -f 1` eth0

            sed -i 's_/usr/bin/dockerd.*_/usr/bin/dockerd {{dockerDns}} -H tcp://127.0.0.1:2375 -H unix:///var/run/docker.sock {{registrySecuritySetting}}_g' /lib/systemd/system/docker.service
            
            {{setupOutboundProxy}}
            systemctl daemon-reload

            systemctl restart docker
            systemctl enable docker
            {{dockerLoginCommand}}

            # Output the node's model specification.
            mkdir -p /config/agent
            echo '{{agentConfig}}' > /config/agent/config.json

            # Update guest-info's network information in vSphere.
            touch /etc/vmware-tools/tools.conf
            printf '[guestinfo]\nprimary-nics=eth*\nexclude-nics=docker*,veth*' > /etc/vmware-tools/tools.conf
            /usr/bin/vmware-toolbox-cmd info update network

            touch /config/concord/config-local/concord.config

            # Adding the genesis block
            mkdir -p /config/concord/config-public
            echo '{{genesis}}' > /config/concord/config-public/genesis.json

            docker run -d --name=agent --restart=always -v /config:/config -v /var/run/docker.sock:/var/run/docker.sock -p 8546:8546 {{agentImage}}
            echo 'done'
            """.trimIndent()
                    .replace("{{dockerLoginCommand}}", containerRegistry.toRegistryLoginCommand())
                    .replace("{{registrySecuritySetting}}", containerRegistry.toRegistrySecuritySetting())
                    .replace("{{agentImage}}", URI.create(containerRegistry.address).authority + "/" + agentImageName)
                    .replace("{{genesis}}", GenesisSerializer.toJson(genesis))
                    .replace("{{agentConfig}}", ConcordAgentConfigurationSerializer.toJson(configuration))
                    .replace("{{networkAddressCommand}}", ipAddress.takeIf { it.isNotBlank() }?.let { networkAddressCommand }?: "")
                    .replace("{{staticIp}}", ipAddress)
                    .replace("{{gateway}}", gateway)
                    .replace("{{dnsSetupCommand}}", dnsSetupCommand)
                    .replace("{{dockerDns}}", dockerDnsSetupCommand)
                    .replace("{{setupOutboundProxy}}", setupOutboundProxy)


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
     * Return appropriate container registry setting for Docker daemon depending on the registry's
     * URL scheme.
     *
     * @return
     *   an option [String] to be passed to Docker engine startup.
     */
    private fun Endpoint.toRegistrySecuritySetting(): String {
        val endpoint = URI.create(address)

        return when (endpoint.scheme.toLowerCase()) {
            "http" -> "--insecure-registry ${endpoint.authority}"
            else -> ""
        }
    }

    /**
     * Retrieve the user-data manifest of the Cloud-Init configuration.
     *
     * @return
     *   user-data manifest as a [String].
     */
    fun userData(): String = script
}

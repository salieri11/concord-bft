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
import com.vmware.blockchain.deployment.v1.ConcordNodeIdentifier
import com.vmware.blockchain.deployment.v1.ConcordComponent
import com.vmware.blockchain.deployment.v1.ConcordModelSpecification
import com.vmware.blockchain.deployment.v1.ConfigurationSessionIdentifier
import com.vmware.blockchain.deployment.v1.Credential
import com.vmware.blockchain.deployment.v1.Endpoint
import com.vmware.blockchain.deployment.v1.LogManagement
import com.vmware.blockchain.deployment.v1.OutboundProxyInfo
import com.vmware.blockchain.deployment.v1.Wavefront
import kotlinx.serialization.modules.serializersModuleOf

import java.util.UUID

/**
 * Initialization script run either on first-boot of a deployed virtual machine.
 */
class CloudInitConfiguration(
    containerRegistry: Endpoint,
    model: ConcordModelSpecification,
    ipAddress: String,
    gateway: String,
    nameServers: List<String>,
    subnet: Int,
    clusterId: ConcordClusterIdentifier,
    private val replicaId: ConcordNodeIdentifier,
    nodeId: Int,
    configGenId: ConfigurationSessionIdentifier,
    configServiceEndpoint: Endpoint,
    configServiceRestEndpoint: Endpoint,
    outboundProxy: OutboundProxyInfo,
    logManagements : List<LogManagement>,
    wavefront: Wavefront,
    private val consortium: String
) {
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
                            ";echo -e '[Service]\\nEnvironment=\"HTTP_PROXY=http://${outboundProxy.httpHost}:${outboundProxy.httpPort}\"\\nEnvironment=\"HTTPS_PROXY=http://${outboundProxy.httpsHost}:${outboundProxy.httpsPort}\"\\nEnvironment=\"NO_PROXY=localhost,127.0.0.1\"' > /etc/systemd/system/docker.service.d/http-proxy.conf"
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
            outboundProxyInfo = outboundProxy,
            loggingEnvVariables = logManagements.loggingEnvVariablesSetup(),
            wavefront = Wavefront(url = wavefront.url, token = wavefront.token)
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

            mkdir -p /etc/docker
            echo '{"log-driver": "json-file", "log-opts": { "max-size": "100m", "max-file": "5"}}' > /etc/docker/daemon.json
            systemctl restart docker
            
            # To enable docker on boot.
            systemctl enable docker

            {{dockerLoginCommand}}

            # Output the node's model specification.
            mkdir -p /config/agent
            echo '{{agentConfig}}' > /config/agent/config.json

            # Output node details
            mkdir -p /config/generic
            echo '{{identifiers}}' > /config/generic/identifiers.env

            # Update guest-info's network information in vSphere.
            touch /etc/vmware-tools/tools.conf
            printf '[guestinfo]\nprimary-nics=eth*\nexclude-nics=docker*,veth*' > /etc/vmware-tools/tools.conf
            /usr/bin/vmware-toolbox-cmd info update network

            docker run -d --name=agent --restart=always -v /config:/config -v /var/run/docker.sock:/var/run/docker.sock -p 8546:8546 {{agentImage}}
            echo 'done'
            """.trimIndent()
                    .replace("{{dockerLoginCommand}}", containerRegistry.toRegistryLoginCommand())
                    .replace("{{registrySecuritySetting}}", containerRegistry.toRegistrySecuritySetting())
                    .replace("{{agentImage}}", URI.create(containerRegistry.address).authority + "/" + agentImageName)
                    .replace("{{agentConfig}}", ConcordAgentConfigurationSerializer.toJson(configuration))
                    .replace("{{identifiers}}", identifiers())
                    .replace("{{networkAddressCommand}}", ipAddress.takeIf { it.isNotBlank() }?.let { networkAddressCommand }?: "")
                    .replace("{{staticIp}}", ipAddress)
                    .replace("{{gateway}}", gateway)
                    .replace("{{dnsSetupCommand}}", dnsSetupCommand)
                    .replace("{{dockerDns}}", dockerDnsSetupCommand)
                    .replace("{{setupOutboundProxy}}", setupOutboundProxy)

    /**
     * Provides uuids as key value pairs.
     *
     * @return
     *   key-value pair of uuids as [String].
     */
    private fun identifiers(): String {
        val nodeUuid = UUID(replicaId.high, replicaId.low).toString()
        val builder = StringBuilder()
        builder.append("NODE_UUID")
                .append("=")
                .append(nodeUuid)
                .append("\n")
                .append("CLIENT_GROUP_ID")
                .append("=")
                .append(nodeUuid)
        return builder.toString()
    }

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
     * Returns Logging environment variables as a string to be injected at container deployment time
     *
     * @return
     *   environment variables as a List<String> in the format ["key=value", "foo=bar"]
     */
    private fun List<LogManagement>.loggingEnvVariablesSetup(): List<String> {

        if (this.isNotEmpty()) {
            // Get first configuration for now
            val logManagement = this.first()
            // Split address to see if port was configured in the endpoint
            // e.g. https://hostname:9000
            val address: List<String> = logManagement.endpoint.address.split(":")
            val portInt = address.last().toIntOrNull()
            val port: String
            val hostname: String
            when(portInt) {
                null -> {
                    port = ""
                    hostname = logManagement.endpoint.address
                }
                else -> {
                    port = "$portInt"
                    hostname = address.take(address.size-1).joinToString()
                }
            }
            val bearerToken = logManagement.endpoint.credential.tokenCredential.token
            val logInsightAgentId = logManagement.logInsightAgentId

            val loggingEnvVariables: MutableList<String> = mutableListOf()
            // Add consortiumId and replicaId information
            loggingEnvVariables.add("CONSORTIUM_ID=$consortium")
            val replicaUUID : String = UUID(replicaId.high, replicaId.low).toString()
            loggingEnvVariables.add("REPLICA_ID=$replicaUUID")

            // Add logging configurations
            loggingEnvVariables.add("LOG_DESTINATION=${logManagement.destination.name}")
            when(logManagement.destination) {
                LogManagement.Type.LOG_INTELLIGENCE -> {
                    loggingEnvVariables.add("LINT_AUTHORIZATION_BEARER=$bearerToken")
                    loggingEnvVariables.add("LINT_ENDPOINT_URL=$hostname")
                }
                LogManagement.Type.LOG_INSIGHT -> {
                    loggingEnvVariables.add("LOG_INSIGHT_HOST=$hostname")
                    loggingEnvVariables.add("LOG_INSIGHT_PORT=$port")
                    loggingEnvVariables.add("LOG_INSIGHT_USERNAME=" +
                            logManagement.endpoint.credential.passwordCredential.username)
                    loggingEnvVariables.add("LOG_INSIGHT_PASSWORD=" +
                            logManagement.endpoint.credential.passwordCredential.password)
                    loggingEnvVariables.add("LOG_INSIGHT_AGENT_ID=$logInsightAgentId")
                }
            }

            return loggingEnvVariables
        }
        return emptyList()
    }

    /**
     * Retrieve the user-data manifest of the Cloud-Init configuration.
     *
     * @return
     *   user-data manifest as a [String].
     */
    fun userData(): String = script
}

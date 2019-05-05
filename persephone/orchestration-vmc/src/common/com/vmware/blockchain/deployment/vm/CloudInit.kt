/* **************************************************************************
 * Copyright (c) 2019 VMware, Inc.  All rights reserved. VMware Confidential
 * *************************************************************************/
package com.vmware.blockchain.deployment.vm

import com.vmware.blockchain.deployment.http.JsonSerializer
import com.vmware.blockchain.deployment.model.ConcordComponent
import com.vmware.blockchain.deployment.model.ConcordModelSpecification
import com.vmware.blockchain.deployment.model.core.Credential
import com.vmware.blockchain.deployment.model.core.Endpoint
import com.vmware.blockchain.deployment.model.ethereum.Genesis
import kotlinx.serialization.modules.serializersModuleOf
import java.util.Base64

/**
 * Initialization script run either on first-boot of a deployed virtual machine.
 */
class InitScript(
    containerRegistry: Endpoint,
    model: ConcordModelSpecification,
    genesis: Genesis,
    concordConfiguration: String,
    ipAddress: String,
    gateway: String
) {

    object GenesisSerializer
        : JsonSerializer(serializersModuleOf(Genesis::class, Genesis.serializer()))

    /** Consolidated Docker PULL command. */
    private val dockerPullCommand: String = model.components.asSequence()
            .filter { it.type == ConcordComponent.Type.DOCKER_IMAGE }
            .map { "docker pull ${containerRegistry.address.authority}/${it.name}" }
            .joinToString(separator = "\n", postfix = "\n")

    private val script =
            """
            #!/bin/sh
            echo -e "c0nc0rd\nc0nc0rd" | /bin/passwd
            route add default gw `ip route show | grep "dev eth0" | grep -v kernel | grep -v default | cut -d' ' -f 1` eth0
            systemctl start docker
            systemctl enable docker
            {{dockerLoginCommand}}
            {{dockerPullCommand}}

            # Output the node's configuration.
            mkdir -p /concord/config-local
            mkdir -p /concord/config-public
            echo '{{staticIp}}' > /concord/ipaddr
            echo '{{gateway}}' > /concord/gateway
           
            tdnf install netmgmt -y
            netmgr ip4_address --set --interface eth0 --mode static --addr {{staticIp}}/24 --gateway {{gateway}}

            touch /concord/config-local/concord.config
            echo '{{concordConfiguration}}' > /concord/config-local/concord.config
            echo '{{genesis}}' > /concord/config-public/genesis.json
            docker run -d --name=concord -v /concord/config-local:/concord/config-local -v /concord/config-public:/concord/config-public -p 5458:5458 -p 3501-3505:3501-3505/udp registry-1.docker.io/vmwblockchain/concord-core:latest /bin/bash -c "export LD_LIBRARY_PATH=${'$'}LD_LIBRARY_PATH:/usr/local/lib && /concord/concord -c /concord/config-local/concord.config"
            docker run -d --name=ethrpc -p 8545:8545 --network host registry-1.docker.io/vmwblockchain/ethrpc:latest
            echo 'done'
            """.trimIndent()
                    .replace("{{dockerLoginCommand}}", containerRegistry.toRegistryLoginCommand())
                    .replace("{{dockerPullCommand}}", dockerPullCommand)
                    .replace("{{concordConfiguration}}", concordConfiguration)
                    .replace("{{genesis}}", GenesisSerializer.toJson(genesis))
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
     * Express the content of the [InitScript] instance as a base64-encoded [ByteArray].
     */
    fun base64(): ByteArray = Base64.getEncoder().encode(script.toByteArray(Charsets.UTF_8))
}

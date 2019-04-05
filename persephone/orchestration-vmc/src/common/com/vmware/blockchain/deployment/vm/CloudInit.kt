/* **************************************************************************
 * Copyright (c) 2019 VMware, Inc.  All rights reserved. VMware Confidential
 * *************************************************************************/
package com.vmware.blockchain.deployment.vm

import com.vmware.blockchain.deployment.model.ConcordComponent
import com.vmware.blockchain.deployment.model.ConcordModelSpecification
import java.util.Base64

/**
 * Initialization script run either on first-boot of a deployed virtual machine.
 */
class InitScript(val model: ConcordModelSpecification) {

    private val dockerPullCommand: String = model.components.asSequence()
            .filter { it.type == ConcordComponent.Type.DOCKER_IMAGE }
            .map { "docker pull ${it.name}" }
            .joinToString(separator = "\n", postfix = "\n")

    private val script =
            """
            #!/bin/sh
            echo -e "c0nc0rd\nc0nc0rd" | /bin/passwd
            route add default gw `ip route show | grep "dev eth0" | grep -v kernel | grep -v default | cut -d' ' -f 1` eth0
            systemctl start docker
            systemctl enable docker
            docker login -u blockchainrepositoryreader -p 'j4jshdh${'$'}@ED2R${'$'}*Trf8'
            {{dockerPullCommand}}
            # Create additional user for copying over the config files.
            useradd vmwuser1 -s /bin/bash -m
            echo "vmwuser1:c0nc0rd" | chpasswd
            """.trimIndent().replace("{{dockerPullCommand}}", dockerPullCommand)

    /**
     * Express the content of the [InitScript] instance as a base64-encoded [ByteArray].
     */
    fun base64(): ByteArray = Base64.getEncoder().encode(script.toByteArray(Charsets.UTF_8))
}

/* **************************************************************************
 * Copyright (c) 2019 VMware, Inc.  All rights reserved. VMware Confidential
 * *************************************************************************/
package com.vmware.blockchain.deployment.vm

import java.nio.charset.StandardCharsets
import java.util.Base64

class InitScript {

    private val script =
            """
            #!/bin/sh
            echo -e "c0nc0rd\nc0nc0rd" | /bin/passwd
            route add default gw `ip route show | grep "dev eth0" | grep -v kernel | grep -v default | cut -d' ' -f 1` eth0
            systemctl start docker
            systemctl enable docker
            docker login -u blockchainrepositoryreader -p 'j4jshdh${'$'}@ED2R${'$'}*Trf8'
            docker pull vmwblockchain/concord-core:latest
            #Create additional user for copying over the config files.
            sudo useradd vmwuser1 -s /bin/bash -m
            echo "vmwuser1:c0nc0rd" | sudo chpasswd
            """.trimIndent()

    fun base64(): ByteArray = Base64.getEncoder().encode(script.toByteArray(StandardCharsets.UTF_8))
}

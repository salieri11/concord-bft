// Copyright (c) 2020 Digital Asset (Switzerland) GmbH and/or its affiliates. All rights reserved.

package com.digitalasset.daml.on.vmware.write.service.bft

import java.nio.file.Paths

object Main extends App {
  var bftClient: Option[BftConcordClientPoolJni] = None

  try {
    bftClient = Some(new BftConcordClientPoolJni(Paths.get("doesnotmatter")))
    bftClient.foreach { bftClient =>
      println(bftClient.currentHealth)
      println(
        bftClient
          .sendRequest(
            Array(0x0),
            timeoutMillis = 30000,
            preExecute = false,
            correlationId = "doesnotmatter"))
    }
  } finally {
    bftClient.foreach(_.close())
  }
}

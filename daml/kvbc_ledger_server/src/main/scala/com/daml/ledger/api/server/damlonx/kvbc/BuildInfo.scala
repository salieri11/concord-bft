// Copyright (c) 2019 Digital Asset (Switzerland) GmbH and/or its affiliates. All rights reserved.
package com.daml.ledger.api.server.damlonx.kvbc

import java.io.{BufferedReader, InputStreamReader}

object BuildInfo {
  val Version: String = {
    Option(this.getClass.getClassLoader.getResourceAsStream("COMPONENT-VERSION")).fold {
      "{component version not found on classpath}"
    } { is =>
      try {
        val reader = new BufferedReader(new InputStreamReader(is))
        reader.lines().reduce("", (t: String, u: String) => t + u).trim

      } finally {
        is.close()
      }
    }
  }
}

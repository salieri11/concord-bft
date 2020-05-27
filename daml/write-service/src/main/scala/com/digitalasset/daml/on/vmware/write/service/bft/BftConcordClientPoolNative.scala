// Copyright (c) 2020 Digital Asset (Switzerland) GmbH and/or its affiliates. All rights reserved.

package com.digitalasset.daml.on.vmware.write.service.bft

import java.nio.file.Path

import com.daml.metrics.Metrics

/**
  * The native implementation of BFT Concord Client Pool is thread-safe.
  */
private[bft] class BftConcordClientPoolNative(configPath: Path, metrics: Metrics)
    extends AutoCloseable {

  @native def sendRequestNative(
      request: Array[Byte],
      timeoutMillis: Long,
      flag: Int,
      correlationId: String): Int

  @native def currentHealthNative: Int

  @native override def close(): Unit
}

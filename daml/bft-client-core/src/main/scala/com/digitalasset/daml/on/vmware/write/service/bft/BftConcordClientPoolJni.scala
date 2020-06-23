// Copyright (c) 2020 Digital Asset (Switzerland) GmbH and/or its affiliates. All rights reserved.

package com.digitalasset.daml.on.vmware.write.service.bft

import java.nio.file.Path

import ch.jodersky.jni.nativeLoader

/**
  * The native implementation of BFT Concord Client Pool is thread-safe.
  */
@nativeLoader("bft-client-native0")
private[bft] class BftConcordClientPoolJni(configPath: Path) extends BftConcordClientPoolCore {

  @native def sendRequest(
      request: Array[Byte],
      timeoutMillis: Long,
      preExecute: Boolean,
      correlationId: String): Int

  @native def currentHealth: Int

  @native override def close(): Unit

  @native private[this] def initialize(configPath: String): Short

  //noinspection ScalaUnusedSymbol
  // Used by native code
  private val nativeHandle: Short = initialize(configPath.toAbsolutePath.toString)
}

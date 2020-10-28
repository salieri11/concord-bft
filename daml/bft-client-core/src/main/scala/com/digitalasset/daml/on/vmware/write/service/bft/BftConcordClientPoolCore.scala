// Copyright (c) 2020 Digital Asset (Switzerland) GmbH and/or its affiliates. All rights reserved.

package com.digitalasset.daml.on.vmware.write.service.bft

/**
  * The native implementation of BFT Concord Client Pool is thread-safe.
  */
private[bft] trait BftConcordClientPoolCore extends AutoCloseable {

  def currentHealth: Int

  def sendRequest(
      request: Array[Byte],
      timeoutMillis: Long,
      preExecute: Boolean,
      correlationId: String
  ): Int
}

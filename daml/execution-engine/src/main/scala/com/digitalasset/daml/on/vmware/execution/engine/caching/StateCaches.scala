// Copyright (c) 2020 Digital Asset (Switzerland) GmbH and/or its affiliates. All rights reserved.

package com.digitalasset.daml.on.vmware.execution.engine.caching

import com.daml.caching.WeightedCache
import com.daml.ledger.participant.state.kvutils.DamlKvutils.{DamlStateKey, DamlStateValue}
import com.daml.ledger.participant.state.kvutils.caching.`Message Weight`
import com.daml.ledger.validator.caching.CachingDamlLedgerStateReader.StateCache
import com.digitalasset.daml.on.vmware.execution.engine.metrics.ValidatorCacheMetrics
import org.slf4j.LoggerFactory

private[engine] object StateCaches {
  val DefaultCacheSize: Int = 256 * 1024 * 1024

  private val logger = LoggerFactory.getLogger(this.getClass)

  def determineCacheSize(): Int = {
    val cacheSizeEnv = System.getenv("KVBC_VALIDATOR_CACHE_SIZE")
    if (cacheSizeEnv == null) {
      logger.warn(s"KVBC_VALIDATOR_CACHE_SIZE unset, defaulting to $DefaultCacheSize bytes")
      DefaultCacheSize
    } else {
      cacheSizeEnv.toInt
    }
  }

  def createDefault(metrics: ValidatorCacheMetrics): StateCache = {
    val cacheSize = determineCacheSize()
    metrics.stateValueCache.synchronized {
      WeightedCache.from[DamlStateKey, DamlStateValue](
        WeightedCache.Configuration(maximumWeight = cacheSize),
        metrics.stateValueCache)
    }
  }
}

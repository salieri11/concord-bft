package com.digitalasset.daml.on.vmware.execution.engine.caching

import com.codahale.metrics.MetricRegistry
import com.daml.caching.{Cache, WeightedCache}
import com.daml.ledger.participant.state.kvutils.caching.`Message Weight`
import com.daml.ledger.participant.state.kvutils.DamlKvutils.{DamlStateKey, DamlStateValue}
import com.daml.metrics.ValidatorCacheMetrics
import org.slf4j.LoggerFactory

private[engine] object StateCaches {
  // TODO Remove once in SDK
  type StateCache = Cache[DamlStateKey, DamlStateValue]

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

  def createDefault(metricRegistry: MetricRegistry): StateCache = {
    val cacheSize = determineCacheSize()
    WeightedCache.from[DamlStateKey, DamlStateValue](
      WeightedCache.Configuration(maximumWeight = cacheSize),
      ValidatorCacheMetrics.create(metricRegistry))
  }
}

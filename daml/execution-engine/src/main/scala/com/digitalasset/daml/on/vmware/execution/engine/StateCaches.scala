package com.digitalasset.daml.on.vmware.execution.engine

import com.codahale.metrics.MetricRegistry
import com.daml.ledger.participant.state.kvutils.DamlKvutils.{DamlStateKey, DamlStateValue}
import com.github.blemale.scaffeine.{Cache, Scaffeine}
import org.slf4j.LoggerFactory

import scala.concurrent.duration._

private[engine] object StateCaches {
  type StateCache = Cache[DamlStateKey, DamlStateValue]

  val DefaultCacheSize: Int = 256 * 1024 * 1024
  val DefaultExpiry: Duration = 1.hour

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
    Scaffeine()
      .recordStats({ () =>
        new KVBCMetricsStatsCounter(metricRegistry)
      })
      .expireAfterWrite(DefaultExpiry)
      .maximumWeight(cacheSize)
      .weigher[DamlStateKey, DamlStateValue] {
        case (key: DamlStateKey, value: DamlStateValue) =>
          key.getSerializedSize + value.getSerializedSize
      }
      .build[DamlStateKey, DamlStateValue]
  }
}

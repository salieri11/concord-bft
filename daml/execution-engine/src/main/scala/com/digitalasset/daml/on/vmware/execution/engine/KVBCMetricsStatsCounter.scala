package com.digitalasset.daml.on.vmware.execution.engine

import com.codahale.metrics.MetricRegistry
import com.github.benmanes.caffeine.cache.stats.{CacheStats, StatsCounter}

class KVBCMetricsStatsCounter(registry: MetricRegistry) extends StatsCounter {
  private val prefix = "daml.validator.cache"
  private val hits = registry.counter(s"$prefix.hits")
  private val misses = registry.counter(s"$prefix.misses")
  private val totalLoadTime = registry.timer(s"$prefix.loads")
  private val loadSuccessCount = registry.counter(s"$prefix.loads-success")
  private val loadFailureCount = registry.counter(s"$prefix.loads-failure")
  private val evictionCount = registry.counter(s"$prefix.evictions")
  private val evictionWeight = registry.counter(s"$prefix.evictions-weight")

  override def recordHits(count: Int): Unit = hits.inc(count)
  override def recordMisses(count: Int): Unit = misses.inc(count)
  override def recordLoadSuccess(loadTime: Long): Unit = loadSuccessCount.inc()
  override def recordLoadFailure(loadTime: Long): Unit = loadFailureCount.inc()
  override def recordEviction(): Unit = evictionCount.inc()
  override def snapshot(): CacheStats =
    new CacheStats(
      hits.getCount,
      misses.getCount,
      loadSuccessCount.getCount,
      loadFailureCount.getCount,
      totalLoadTime.getCount,
      evictionCount.getCount,
      evictionWeight.getCount
    )
  override def toString: String = snapshot().toString
}

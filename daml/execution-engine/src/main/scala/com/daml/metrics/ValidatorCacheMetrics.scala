package com.daml.metrics

import com.codahale.metrics.MetricRegistry

object ValidatorCacheMetrics {
  val Prefix: MetricName = MetricName.DAML :+ "validator" :+ "cache"
  def create(metricRegistry: MetricRegistry) = new CacheMetrics(metricRegistry, Prefix)
}

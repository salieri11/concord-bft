package com.digitalasset.daml.on.vmware.execution.engine.metrics

import com.codahale.metrics.MetricRegistry
import com.daml.metrics.{CacheMetrics, MetricName}

class ValidatorCacheMetrics(metricRegistry: MetricRegistry) {
  val Prefix: MetricName = MetricName.DAML :+ "validator" :+ "cache"

  val stateValueCache: CacheMetrics = new CacheMetrics(metricRegistry, Prefix)
  val stateValueCacheForPreExecution: CacheMetrics =
    new CacheMetrics(metricRegistry, Prefix :+ "pre_execution")
}

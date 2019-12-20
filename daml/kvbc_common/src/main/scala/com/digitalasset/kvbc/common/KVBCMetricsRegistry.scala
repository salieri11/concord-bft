package com.digitalasset.kvbc.common

import com.codahale.metrics.jvm.{GarbageCollectorMetricSet, MemoryUsageGaugeSet, ThreadStatesGaugeSet}
import com.codahale.metrics.{MetricRegistry, SharedMetricRegistries}

class KVBCMetricsRegistry(registryName: String) {
  // Set the default registry
  val registry: MetricRegistry = SharedMetricRegistries.getOrCreate(registryName)

  // Register JVM related metrics.
  (new GarbageCollectorMetricSet).getMetrics.forEach { (k, m) =>
    registry.register(s"jvm.gc.$k", m)
  }
  (new MemoryUsageGaugeSet).getMetrics.forEach { (k, m) =>
    registry.register(s"jvm.mem.$k", m)
  }
  (new ThreadStatesGaugeSet).getMetrics.forEach { (k, m) =>
    registry.register(s"jvm.threads.$k", m)
  }

}
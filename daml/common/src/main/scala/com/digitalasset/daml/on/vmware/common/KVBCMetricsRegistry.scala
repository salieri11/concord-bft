// Copyright (c) 2020 Digital Asset (Switzerland) GmbH and/or its affiliates. All rights reserved.

package com.digitalasset.daml.on.vmware.common

import com.codahale.metrics.jvm.{GarbageCollectorMetricSet, MemoryUsageGaugeSet, ThreadStatesGaugeSet}
import com.codahale.metrics.{MetricRegistry, SharedMetricRegistries}

class KVBCMetricsRegistry(private[vmware] val registryName: String) extends SharedMetricRegistryCloseable(registryName) {
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
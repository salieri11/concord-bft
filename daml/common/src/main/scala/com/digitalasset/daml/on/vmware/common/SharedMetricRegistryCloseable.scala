package com.digitalasset.daml.on.vmware.common

import com.codahale.metrics.SharedMetricRegistries

private[vmware] class SharedMetricRegistryCloseable(key: String) extends AutoCloseable {
  final override def close(): Unit = SharedMetricRegistries.remove(key)
}

private[vmware] object SharedMetricRegistryCloseable {
  def apply(key: String) = new SharedMetricRegistryCloseable(key)
}
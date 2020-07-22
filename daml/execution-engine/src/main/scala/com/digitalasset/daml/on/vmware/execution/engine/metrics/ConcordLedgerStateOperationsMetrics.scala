// Copyright (c) 2020 Digital Asset (Switzerland) GmbH and/or its affiliates. All rights reserved.

package com.digitalasset.daml.on.vmware.execution.engine.metrics

import com.codahale.metrics.{Histogram, MetricRegistry, Timer}
import com.daml.metrics.MetricName

private[engine] class ConcordLedgerStateOperationsMetrics(metricRegistry: MetricRegistry) {
  val Prefix: MetricName = MetricName.DAML :+ "validator"

  val readCompletionTime: Timer = metricRegistry.timer(Prefix :+ "key_read")
  val bytesWritten: Histogram = metricRegistry.histogram(Prefix :+ "bytes_written")
}

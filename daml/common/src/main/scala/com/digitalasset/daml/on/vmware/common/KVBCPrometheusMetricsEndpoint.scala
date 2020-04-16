// Copyright (c) 2020 Digital Asset (Switzerland) GmbH and/or its affiliates. All rights reserved.

package com.digitalasset.daml.on.vmware.common

import com.codahale.metrics.MetricRegistry
import io.prometheus.client.CollectorRegistry
import io.prometheus.client.dropwizard.DropwizardExports
import io.prometheus.client.exporter.MetricsServlet
import org.eclipse.jetty.servlet.ServletContextHandler
import org.eclipse.jetty.servlet.ServletHolder

import scala.collection.mutable

import com.daml.ledger.participant.state.pkvutils.metrics.util.ExtendedDropwizardExports

object KVBCPrometheusMetricsEndpoint {

  def createEndpoint(registry: MetricRegistry, context: ServletContextHandler): AutoCloseable =
    new AutoCloseable {
      private[this] val collector = new ExtendedDropwizardExports(registry)

      CollectorRegistry.defaultRegistry.register(collector)
      context.addServlet(new ServletHolder(new MetricsServlet()), "/metrics")

      override def close(): Unit = CollectorRegistry.defaultRegistry.unregister(collector)
    }

  def createEndpoint(registries: List[MetricRegistry], context: ServletContextHandler): AutoCloseable =
    new AutoCloseable {
      private[this] val collectors = mutable.ListBuffer[DropwizardExports]()

      registries.foreach(registry =>
        CollectorRegistry.defaultRegistry.register {
          val collector = new DropwizardExports(registry)
          collectors += collector
          collector
        })

      context.addServlet(new ServletHolder(new MetricsServlet()), "/metrics")

      override def close(): Unit = collectors.foreach(CollectorRegistry.defaultRegistry.unregister)
    }
}

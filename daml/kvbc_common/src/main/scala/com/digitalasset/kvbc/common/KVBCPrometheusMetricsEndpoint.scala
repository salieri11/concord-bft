package com.digitalasset.kvbc.common

import com.codahale.metrics.MetricRegistry
import io.prometheus.client.CollectorRegistry
import io.prometheus.client.dropwizard.DropwizardExports
import io.prometheus.client.exporter.MetricsServlet

import org.eclipse.jetty.servlet.ServletContextHandler
import org.eclipse.jetty.servlet.ServletHolder

object KVBCPrometheusMetricsEndpoint {

  def createEndpoint(registry: MetricRegistry, context: ServletContextHandler) = {
      CollectorRegistry.defaultRegistry.register(new DropwizardExports(registry))
      context.addServlet(new ServletHolder(new MetricsServlet()), "/metrics")
  }

  def createEndpoint(registries: List[MetricRegistry], context: ServletContextHandler) = {
      registries.foreach(registry =>
        CollectorRegistry.defaultRegistry.register(new DropwizardExports(registry)))
      context.addServlet(new ServletHolder(new MetricsServlet()), "/metrics")
  }

}

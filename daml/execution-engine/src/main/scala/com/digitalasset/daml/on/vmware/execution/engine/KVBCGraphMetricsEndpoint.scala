package com.digitalasset.daml.on.vmware.execution.engine

import java.net.URI

import com.codahale.metrics.health.HealthCheckRegistry
import com.codahale.metrics.servlets.{AdminServlet, HealthCheckServlet, MetricsServlet}
import com.codahale.metrics.MetricRegistry
import org.eclipse.jetty.servlet.ServletContextHandler
import org.eclipse.jetty.util.resource.Resource
import org.eclipse.jetty.servlet.{DefaultServlet, ServletHolder}

final class KVBCGraphMetricsEndpoint(registry: MetricRegistry, context: ServletContextHandler) {

  val indexFile = "index.html"
  val resourceUri: URI =
    new URI(this.getClass.getResource(s"/assets/$indexFile").toString().dropRight(indexFile.length))
  val resource: Resource = Resource.newResource(resourceUri)
  context.setBaseResource(resource)

  val staticHolder = new ServletHolder("default", classOf[DefaultServlet])
  staticHolder.setInitParameter("dirAllowed", "false")
  context.addServlet(staticHolder, "/")
  context.setWelcomeFiles(Array("index.html"))

  val holder = new ServletHolder(new AdminServlet())
  holder.setInitOrder(0)
  context.setAttribute(MetricsServlet.METRICS_REGISTRY, registry)
  context.setAttribute(HealthCheckServlet.HEALTH_CHECK_REGISTRY, new HealthCheckRegistry)
  context.addServlet(holder, "/admin/*")

}

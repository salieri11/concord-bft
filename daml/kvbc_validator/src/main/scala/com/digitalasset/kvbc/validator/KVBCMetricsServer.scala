package com.digitalasset.kvbc.validator

import java.net.URI

import com.codahale.metrics.health.HealthCheckRegistry
import com.codahale.metrics.jvm.{GarbageCollectorMetricSet, MemoryUsageGaugeSet, ThreadStatesGaugeSet}
import com.codahale.metrics.servlets.{AdminServlet, HealthCheckServlet, MetricsServlet}
import com.codahale.metrics.{MetricRegistry, SharedMetricRegistries}
import org.eclipse.jetty.server.Server
import org.eclipse.jetty.servlet.ServletContextHandler
import org.eclipse.jetty.util.resource.Resource
import org.eclipse.jetty.servlet.{DefaultServlet, ServletHolder}

object KVBCMetricsServer {
  // Set the default registry
  val registry: MetricRegistry = SharedMetricRegistries.setDefault("kvutils")

  def run(): Server = {

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

    // Start HTTP server to serve the metrics.
    val server = new Server(55001)
    val context = new ServletContextHandler()
    server.setHandler(context)
    context.setContextPath("/")
    val indexFile = "index.html"
    val resourceUri: URI =
      new URI(KVBCMetricsServer.getClass.getResource(s"/assets/$indexFile").toString().dropRight(indexFile.length))
    val resource: Resource = Resource.newResource(resourceUri)
    context.setBaseResource(resource)
    val staticHolder = new ServletHolder("default", classOf[DefaultServlet])
    staticHolder.setInitParameter("dirAllowed", "false")
    context.addServlet(staticHolder, "/")
    context.setWelcomeFiles(Array("index.html"))

    val holder = new ServletHolder(new AdminServlet())
    holder.setInitOrder(0)
    context.setAttribute(MetricsServlet.METRICS_REGISTRY, SharedMetricRegistries.getDefault)
    context.setAttribute(HealthCheckServlet.HEALTH_CHECK_REGISTRY, new HealthCheckRegistry)
    context.addServlet(holder, "/admin/*")

    server.start()
    server
  }
}

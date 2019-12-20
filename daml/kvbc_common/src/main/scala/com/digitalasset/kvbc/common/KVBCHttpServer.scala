package com.digitalasset.kvbc.common

import org.eclipse.jetty.server.Server
import org.eclipse.jetty.servlet.ServletContextHandler

final class KVBCHttpServer {
  
  // Start HTTP server to serve the metrics.
  private val server = new Server(55001)
  val context: ServletContextHandler = new ServletContextHandler()
  server.setHandler(context)
  context.setContextPath("/")

  def start() =
    server.start()
  
  def stop() = {
    server.stop()
    server.join()
  }
}

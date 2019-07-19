// Copyright (c) 2019 Digital Asset (Switzerland) GmbH and/or its affiliates. All rights reserved.

package com.digitalasset.kvbc.validator

import io.grpc.{Server, ServerBuilder}
import com.digitalasset.kvbc.daml_validator._
import org.slf4j.LoggerFactory

import scala.concurrent.ExecutionContext

object KVBCValidatorMain extends App {
  val server = new KVBCValidatorServer(ExecutionContext.global)
  server.start()
  server.blockUntilShutdown()
}

class KVBCValidatorServer(executionContext: ExecutionContext) {
  private[this] var server: Server = null
  private[this] val port = 55000
  private val logger = LoggerFactory.getLogger(this.getClass)

  def start(): Unit = {
    server = ServerBuilder.forPort(port)
      .addService(ValidationServiceGrpc.bindService(new KVBCValidator, executionContext))
      .build.start
    logger.info("Server started, listening on " + port)
    sys.addShutdownHook {
      System.err.println("Shutting down...")
      stop()
    }
  }

  def stop(): Unit = {
    if (server != null) {
      server.shutdown()
    }
  }

  def blockUntilShutdown(): Unit = {
    if (server != null) {
      server.awaitTermination()
    }
  }
}

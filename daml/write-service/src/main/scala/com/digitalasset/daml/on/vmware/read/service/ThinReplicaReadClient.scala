// Copyright (c) 2020 Digital Asset (Switzerland) GmbH and/or its affiliates. All rights reserved.

package com.digitalasset.daml.on.vmware.read.service

import akka.NotUsed
import akka.stream.scaladsl.{RestartSource, Source}
import com.codahale.metrics.MetricRegistry
import com.digitalasset.daml.on.vmware.common.Constants
import com.digitalasset.daml.on.vmware.thin.replica.client.core.{ThinReplicaClient, Update}
import org.slf4j.LoggerFactory
import scala.concurrent.duration._

class ThinReplicaReadClient(
    clientId: String,
    maxFaulty: Short,
    privateKey: String,
    servers: Array[String],
    maxReadDataTimeout: Short,
    maxReadHashTimeout: Short,
    jaegerAgent: String,
    trcCore: ThinReplicaClient,
    metricRegistry: MetricRegistry) {
  import ThinReplicaReadClient._

  private object Metrics {
    val prefix = "daml.trc"
    val getBlockTimer = metricRegistry.timer(s"$prefix.get-block")
  }

  private val logger = LoggerFactory.getLogger(this.getClass)

  val trcCreated = trcCore.initialize(
    clientId,
    maxFaulty,
    privateKey,
    servers,
    maxReadDataTimeout,
    maxReadHashTimeout,
    jaegerAgent)
  if (trcCreated)
    logger.info("Thin Replica Client created")
  else
    logger.error("Failed to create Thin Replica Client")

  def committedBlocks(offset: Long): Source[Block, NotUsed] =
    if (trcCreated)
      createRestartSource(offset)
        .dropWhile { committedTx =>
          committedTx.blockId < offset
        } else
      Source.empty

  private def createRestartSource(offset: Long): Source[Block, NotUsed] =
    RestartSource.withBackoff(
      minBackoff = 1.seconds,
      maxBackoff = 30.seconds,
      randomFactor = 0.2 // adds 20% "noise" to vary the intervals slightly
    ) { () =>
      createSource(offset)
    }

  private def createSource(offset: Long): Source[Block, NotUsed] =
    Source
      .unfoldResource[Block, Boolean](
        // open
        () => {
          logger.info(s"Subscribing to `daml` key/value feed using thin replica, offset=$offset")
          // If subscription is done for the first time, let the TRC
          // figure out the earliest available offset. After pruning
          // this offset may have quite high value.
          // TODO(JM): subscribe should take array of bytes
          if (offset > 0)
            trcCore.subscribe(Constants.fragmentKeyPrefix.toStringUtf8, offset)
          else
            trcCore.subscribe(Constants.fragmentKeyPrefix.toStringUtf8)
        },
        // read
        subsResult =>
          // None return signals end of resource
          if (subsResult)
            Metrics.getBlockTimer.time(() => trcCore.pop)
          else
          None,
        // close
        subsResult =>
          // Do nothing, unsubscribe should not be called here.
          if (!subsResult)
            logger.warn("Failed to subscribe to the thin replica, retrying...")
          else
            logger.warn("Failed to pop an element from the thin replica, retrying...")
      )
}

object ThinReplicaReadClient {
  type Block = Update
}

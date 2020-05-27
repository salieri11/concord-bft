// Copyright (c) 2020 Digital Asset (Switzerland) GmbH and/or its affiliates. All rights reserved.

package com.digitalasset.daml.on.vmware.read.service

import akka.NotUsed
import akka.stream.scaladsl.Source
import com.codahale.metrics.MetricRegistry
import com.digitalasset.daml.on.vmware.common.Constants
import com.digitalasset.daml.on.vmware.thin.replica.client.core.{Library, Update}
import org.slf4j.LoggerFactory

class ThinReplicaReadClient(
    clientId: String,
    maxFaulty: Short,
    privateKey: String,
    servers: Array[String],
    maxReadDataTimeout: Short,
    maxReadHashTimeout: Short,
    jaegerAgent: String,
    metricRegistry: MetricRegistry) {
  import ThinReplicaReadClient._

  private object Metrics {
    val prefix = "daml.trc"
    val getBlockTimer = metricRegistry.timer(s"$prefix.get-block")
  }

  private val logger = LoggerFactory.getLogger(this.getClass)

  val trcCreated = Library.createTRC(
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
              Library.subscribe(Constants.fragmentKeyPrefix.toStringUtf8, offset)
            else
              Library.subscribe(Constants.fragmentKeyPrefix.toStringUtf8)
          },
          // read
          subsResult =>
            // None return signals end of resource
            if (subsResult)
              Metrics.getBlockTimer.time(() => Library.pop)
            else
            None,
          // close
          subsResult =>
            // Do nothing, unsubscribe should not be called here.
            logger.info("Deleting thin replica instance")
        )
        .dropWhile { committedTx =>
          committedTx.blockId < offset
        } else
      Source.empty
}

object ThinReplicaReadClient {
  type Block = Update
}

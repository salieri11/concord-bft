// Copyright (c) 2019 Digital Asset (Switzerland) GmbH and/or its affiliates. All rights reserved.

package com.digitalasset.daml.on.vmware.write.service

import com.digitalasset.daml.on.vmware.thin.replica.client.core.Library
import com.digitalasset.daml.on.vmware.thin.replica.client.core.Update
import org.slf4j.LoggerFactory
import akka.NotUsed
import akka.stream.scaladsl.Source

class TRClient(clientId: String , maxFaulty: Short,
               privateKey: String, servers: Array[String]) {
  import TRClient._

  private val logger = LoggerFactory.getLogger(this.getClass)

  val trcCreated = Library.createTRC(clientId, maxFaulty, privateKey, servers)
  if(trcCreated)
    logger.info("Thin Replica Client created")
  else
    logger.error("Failed to create Thin Replica Client")

  def committedBlocks(offset: Long): Source[Block, NotUsed] =
    if(trcCreated)
      Source
        .unfoldResource[Block, Boolean](
          // open
          () => {
            logger.info(s"Subscribing to `daml` key/value feed using thin replica, offset=$offset")
            // If subscription is done for the first time, let the TRC
            // figure out the earliest available offset. After pruning
            // this offset may have quite high value.
            if (offset > 0)
              Library.subscribe("daml", offset)
            else
              Library.subscribe("daml")},
          // read
          subsResult => 
            // None return signals end of resource
            if(subsResult)
                          Library.pop()
                        else
                          None,
          // close
          subsResult =>
            // Do nothing, unsubscribe should not be called here.
            logger.info("Deleting thin replica instance"))
        .dropWhile { committedTx => committedTx.blockId < offset }
    else
      Source.empty
}

object TRClient {
  type Block = Update
}
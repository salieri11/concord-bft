package com.digitalasset.daml.on.vmware.ledger.api.server

import com.daml.ledger.participant.state.kvutils.app.Runner
import com.daml.resources.{ProgramResource, ResourceOwner}
import org.slf4j.LoggerFactory

object Main {
  private[this] val logger = LoggerFactory.getLogger(this.getClass)

  def main(args: Array[String]): Unit =
    new ProgramResource(new Runner("vDAML Ledger API Server", ConcordLedgerFactory).owner(args))
      .run()
}

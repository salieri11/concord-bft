package com.digitalasset.daml.on.vmware.ledger.api.server

import com.daml.ledger.participant.state.kvutils.app.Runner
import com.daml.resources.ProgramResource

object Main {
  def main(args: Array[String]): Unit =
    new ProgramResource(new Runner("vDAML Ledger API Server", ConcordLedgerFactory).owner(args))
      .run()
}

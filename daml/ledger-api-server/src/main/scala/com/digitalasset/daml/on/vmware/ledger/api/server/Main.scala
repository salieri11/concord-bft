package com.digitalasset.daml.on.vmware.ledger.api.server

import com.daml.ledger.participant.state.kvutils.app.Runner
import com.digitalasset.resources.{ProgramResource, ResourceOwner}
import org.slf4j.LoggerFactory

object Main {
  private[this] val logger = LoggerFactory.getLogger(this.getClass)

  def main(args: Array[String]): Unit = {
    new ProgramResource(new Runner("vDAML Ledger API Server",
                                   ConcordLedgerFactory) {
      override def owner(args: Seq[String]): ResourceOwner[Unit] = {
        val config =
          Cli
            .parse(args.toArray)
            .getOrElse(sys.exit(1))
        logger.info(
          s"""Initialized vDAML ledger api server: version=${BuildInfo.Version}
             |participantId=${config.participantId} replicas=${config.replicas}
             |jdbcUrl=${config.jdbcUrl}
             |dar_file(s)=${args.drop(2).mkString("(", ";", ")")}""".stripMargin
            .replaceAll("\n", " "))
        super.owner(ConcordLedgerFactory.toKVUtilsAppConfig(config))
      }
    }.owner(args)).run()
  }
}

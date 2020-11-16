// Copyright (c) 2020 Digital Asset (Switzerland) GmbH and/or its affiliates. All rights reserved.

package com.digitalasset.daml.on.vmware.participant.state

import akka.NotUsed
import akka.stream.scaladsl.Source
import com.daml.ledger.participant.state.kvutils.OffsetBuilder
import com.daml.ledger.participant.state.pkvutils.api.{KeyValueLedgerReader, LedgerBlockContent}
import com.daml.ledger.participant.state.v1.{LedgerId, Offset}
import com.digitalasset.daml.on.vmware.thin.replica.client.core.Update
import com.daml.ledger.api.health.{HealthStatus, Healthy}
import com.daml.ledger.telemetry.Tracer
import com.digitalasset.daml.on.vmware.read.service.ThinReplicaReadClient
import com.google.protobuf.ByteString
import io.opentelemetry.trace.Span
import org.slf4j.LoggerFactory

class ConcordKeyValueLedgerReader(
    committedBlocksSource: Long => Source[Update, NotUsed],
    override val retrieveLedgerId: LedgerId,
    fetchCurrentHealth: () => HealthStatus = () => Healthy)
    extends KeyValueLedgerReader {
  private[this] val logger = LoggerFactory.getLogger(this.getClass)

  def events(offset: Option[Offset]): Source[LedgerBlockContent, NotUsed] = {
    val beginFromBlockId =
      offset
        .map(OffsetBuilder.highestIndex)
        .getOrElse(ConcordKeyValueLedgerReader.StartIndex)

    committedBlocksSource(beginFromBlockId)
      .flatMapConcat { block =>
        if (block.kvPairs.nonEmpty) {
          val span = Tracer
            .spanBuilder(s"${classOf[ConcordKeyValueLedgerReader].getSimpleName}.events")
            .setSpanKind(Span.Kind.CONSUMER)
            .setNoParent()
            .setAttribute("correlationId", block.correlationId)
            .setAttribute("blockId", block.blockId)
            .startSpan()
          try {
            logger.info(
              s"Processing blockId=${block.blockId.toHexString} correlationId=${block.correlationId} size=${block.kvPairs.length}")
            if (logger.isTraceEnabled())
              logger.trace(
                s"keys=[${block.kvPairs.map(_._1).map(ByteString.copyFrom).map(_.toStringUtf8).mkString(",")}]")
            Source.single(
              LedgerBlockContent(
                OffsetBuilder.fromLong(block.blockId),
                block.kvPairs.toSeq.map {
                  case (keyByteArray, valueByteArray) =>
                    (ByteString.copyFrom(keyByteArray), ByteString.copyFrom(valueByteArray))
                },
                span,
              ))
          } finally {
            span.end()
          }
        } else {
          Source.empty
        }
      }
  }

  def currentHealth(): HealthStatus = fetchCurrentHealth()
}

object ConcordKeyValueLedgerReader {
  private[state] val StartIndex: Long = 0

  def create(ledgerId: LedgerId, client: ThinReplicaReadClient): ConcordKeyValueLedgerReader =
    new ConcordKeyValueLedgerReader(client.committedBlocks, ledgerId, () => Healthy)
}

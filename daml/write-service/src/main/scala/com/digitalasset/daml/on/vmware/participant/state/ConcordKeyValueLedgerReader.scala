package com.digitalasset.daml.on.vmware.participant.state

import akka.NotUsed
import akka.stream.scaladsl.Source
import com.daml.ledger.participant.state.kvutils.KVOffset
import com.daml.ledger.participant.state.pkvutils.api.{
  KeyValueLedgerReader,
  LedgerBlockContent
}
import com.daml.ledger.participant.state.v1.{LedgerId, Offset}
import com.digitalasset.daml.on.vmware.thin.replica.client.core.Update
import com.digitalasset.daml.on.vmware.write.service.TRClient
import com.daml.ledger.api.health.{HealthStatus, Healthy}
import com.google.protobuf.ByteString
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
        .map(KVOffset.highestIndex)
        .getOrElse(ConcordKeyValueLedgerReader.StartIndex)

    committedBlocksSource(beginFromBlockId)
      .flatMapConcat { block =>
        if(block.kvPairs.nonEmpty) {
          logger.info(s"Processing blockId=${block.blockId} correlationId=${block.correlationId} size=${block.kvPairs.length}")
          Source.single(
            LedgerBlockContent(
              KVOffset.fromLong(block.blockId),
              block.kvPairs.toSeq.map {
                case (keyByteArray, valueByteArray) =>
                  (ByteString.copyFrom(keyByteArray),
                  ByteString.copyFrom(valueByteArray))
              }
            ))
        } else {
          Source.empty
        }
      }
  }

  def currentHealth(): HealthStatus = fetchCurrentHealth()
}

object ConcordKeyValueLedgerReader {
  private[state] val StartIndex: Long = 0

  def create(ledgerId: LedgerId,
             client: TRClient): ConcordKeyValueLedgerReader =
    new ConcordKeyValueLedgerReader(client.committedBlocks,
                                    ledgerId,
                                    () => Healthy)
}

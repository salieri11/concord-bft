// Copyright (c) 2019 Digital Asset (Switzerland) GmbH and/or its affiliates. All rights reserved.

package com.digitalasset.kvbc_sync_adapter

import com.digitalasset.kvbc.daml_commit._
import com.digitalasset.kvbc.daml_data._
import com.digitalasset.kvbc.daml_events._
import io.grpc.ManagedChannelBuilder
import scala.concurrent.Future
import com.digitalasset.grpc.adapter.ExecutionSequencerFactory
import akka.NotUsed
import akka.stream.scaladsl.Source
import com.digitalasset.grpc.adapter.client.akka._

/**
 * Raw client for the KVBC API
 */
class KVBCClient private(host: String, port: Int) {
  private val ledgerInboundMessageSizeMax: Int = 50 * 1024 * 1024 // 50 MiBytes

  val channel =
    ManagedChannelBuilder
      .forAddress(host, port)
      .maxInboundMessageSize(ledgerInboundMessageSizeMax)
      .usePlaintext()
      .build

  val commitClient = CommitServiceGrpc.stub(channel)
  val dataClient = DataServiceGrpc.stub(channel)
  val eventsClient = EventsServiceGrpc.stub(channel)

  //
  // Commit API
  //
  def commitTransaction(req: CommitRequest): Future[CommitResponse] =
    commitClient.commitTransaction(req)

  //
  // Data API
  //
  def readKeys(req: ReadTransactionRequest): Future[ReadTransactionResponse] =
    dataClient.readTransaction(req)

  def getLatestBlockId(): Future[BlockId] =
    dataClient.getLatestBlockId(GetLatestBlockIdRequest())

  //
  // Events API
  //

  def committedTxs(offset: Long)(implicit esf: ExecutionSequencerFactory): Source[CommittedTx, NotUsed] =
    ClientAdapter
      .serverStreaming(
         CommittedTxsRequest(),
         eventsClient.committedTxs
      )
      .dropWhile { committedTx => committedTx.blockId < offset }

}

object KVBCClient {
  def apply(host: String, port: Int): KVBCClient =
    new KVBCClient(host, port)
}

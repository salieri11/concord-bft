// Copyright (c) 2019 Digital Asset (Switzerland) GmbH and/or its affiliates. All rights reserved.

package com.digitalasset.kvbc_sync_adapter

import com.digitalasset.kvbc.daml_commit._
import com.digitalasset.kvbc.daml_data._
import com.digitalasset.kvbc.daml_events._
import com.digitalasset.grpc.adapter.ExecutionSequencerFactory
import com.digitalasset.grpc.adapter.client.akka._
import io.grpc.{ManagedChannel, ManagedChannelBuilder}

import scala.concurrent.{ExecutionContext, Future}
import scala.concurrent.duration.DurationInt
import akka.NotUsed
import akka.stream.scaladsl.Source


/**
 * Raw client for the KVBC API
 */
class KVBCClient private(host: String, port: Int) (implicit val ec: ExecutionContext) {
  private val ledgerInboundMessageSizeMax: Int = 50 * 1024 * 1024 // 50 MiBytes

  val channel: ManagedChannel =
    ManagedChannelBuilder
      .forAddress(host, port)
      .maxInboundMessageSize(ledgerInboundMessageSizeMax)
      .usePlaintext()
      .build

  val commitClient: CommitServiceGrpc.CommitServiceStub = CommitServiceGrpc.stub(channel)
  val dataClient: DataServiceGrpc.DataServiceStub = DataServiceGrpc.stub(channel)
  val eventsClient: EventsServiceGrpc.EventsServiceStub = EventsServiceGrpc.stub(channel)
  val backOff: RetryStrategy = RetryStrategy.exponentialBackoff(10,100.milliseconds)

  //
  // Commit API
  //
  def commitTransaction(req: CommitRequest): Future[CommitResponse] =
    backOff(n => commitClient.commitTransaction(req))

  //
  // Data API
  //
  def readKeys(req: ReadTransactionRequest): Future[ReadTransactionResponse] =
    backOff(n => dataClient.readTransaction(req))

  def getLatestBlockId: Future[BlockId] =
    backOff(n => dataClient.getLatestBlockId(GetLatestBlockIdRequest()))

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
  def apply(host: String, port: Int)(implicit ec: ExecutionContext): KVBCClient =
    new KVBCClient(host, port)
}

// Copyright (c) 2019 Digital Asset (Switzerland) GmbH and/or its affiliates. All rights reserved.

package com.digitalasset.daml.on.vmware.write.service

import java.time.Duration

import akka.{Done, NotUsed}
import akka.actor.ActorSystem
import akka.stream.Materializer
import akka.stream.scaladsl._
import com.digitalasset.daml.lf.data.{Ref, Time}
import com.digitalasset.daml.lf.data.Ref.Party
import com.digitalasset.daml.lf.data.Time.Timestamp
import com.digitalasset.daml_lf_dev.DamlLf.Archive
import com.digitalasset.grpc.adapter.AkkaExecutionSequencerPool
import com.digitalasset.kvbc.daml_commit._
import com.digitalasset.ledger.api.health.{HealthStatus, Healthy}
import com.digitalasset.ledger.api.domain.PartyDetails

import com.google.protobuf.ByteString

import com.daml.ledger.participant.state.v1._
import com.daml.ledger.participant.state.kvutils._
import org.slf4j.LoggerFactory

import scala.compat.java8.FutureConverters._
import scala.concurrent.ExecutionContext
import scala.util.{Failure, Success}

import java.util.concurrent.{CompletionStage, CompletableFuture, CompletionException}
import java.util.UUID
import io.grpc.{ConnectivityState, StatusRuntimeException, Status}

import com.daml.ledger.participant.state.pkvutils.Defragmenter

import com.daml.ledger.participant.state.pkvutils.protobuf

/**
 * DAML Participant State implementation on top of VMware Blockchain.
 */
@SuppressWarnings(
  Array(
    "org.wartremover.warts.EitherProjectionPartial",
    "org.wartremover.warts.OptionPartial",
    "org.wartremover.warts.Var"
  ))
class KVBCParticipantState(
    thisLedgerId: LedgerId, // Our ledger id.
    participantId: ParticipantId,
    addresses: Array[String],
    useThinReplica: Boolean,
    maxFaultyReplicas: Short,
  )(implicit val mat: Materializer,
    system: ActorSystem)
  extends ReadService
  with WriteService {

  val firstAddr = addresses.head.split(":")
  private val host: String = firstAddr(0)    // KVBC Server hostname
  private val port: Int = firstAddr(1).toInt // KVBC Server port number

  implicit val ec: ExecutionContext = mat.executionContext

  implicit val esf: AkkaExecutionSequencerPool =
    new AkkaExecutionSequencerPool(
      "kvbc-rs-grpc-bridge",
      16)(system)

  def ledgerId: LedgerId = thisLedgerId
  /** The index of the beginning of the commit log */
  private val beginning: Long = 0

  private val logger = LoggerFactory.getLogger(this.getClass)
  val client = KVBCClient(host, port)(ec)
  val trc: Option[TRClient] = 
    if(useThinReplica)
      Some(new TRClient(participantId, maxFaultyReplicas, "", addresses))
    else
      None

  // Make sure the server is ready to receive requests
  while(client.channel.getState(true) != ConnectivityState.READY) {
      logger.info("Waiting for Concord to be ready")
      Thread.sleep(1000)
  }

  /** Allocate a party on the ledger */
  override def allocateParty(
      hint: Option[Party],
      displayName: Option[String],
      submissionId: SubmissionId): CompletionStage[SubmissionResult] = {

    val party = hint.getOrElse(generateRandomParty())

    val submission: DamlKvutils.DamlSubmission =
      KeyValueSubmission.partyToSubmission(submissionId, Some(party), displayName, participantId)

    val commitReq = CommitRequest(
      submission = Envelope.enclose(submission),
      participantId = participantId.toString,
      correlationId = submissionId.toString
    )

    logger.info(s"Allocating party, party=$party correlationId=$submissionId")

    // FIXME(JM): Properly queue the transactions and execute in sequence from one place.
    submit(submissionId, commitReq)
  }


  private def generateRandomParty(): Ref.Party =
    Ref.Party.assertFromString(s"party-${UUID.randomUUID().toString.take(8)}")

  /** Upload DAML-LF packages to the ledger */
  override def uploadPackages(
      submissionId: SubmissionId,
      archives: List[Archive],
      sourceDescription: Option[String]): CompletionStage[SubmissionResult] = {

    val submission: DamlKvutils.DamlSubmission =
      KeyValueSubmission.archivesToSubmission(submissionId, archives, sourceDescription.getOrElse(""), participantId)

    val envelope = Envelope.enclose(submission)
    val commitReq = CommitRequest(
      submission = envelope,
      participantId = participantId.toString,
      correlationId = submissionId
    )

    logger.info(s"""Uploading package(s), packages=[${archives.map(_.getHash).mkString(",")}] correlationId=$submissionId 
          |envelopeSize=${envelope.size}""".stripMargin.stripLineEnd)

    // FIXME(JM): Properly queue the transactions and execute in sequence from one place.
    submit(submissionId, commitReq)
  }

  private def submit(submissionId: String, commitReq: CommitRequest) : CompletionStage[SubmissionResult] = 
    client
      .commitTransaction(commitReq)
      .toJava
      .toCompletableFuture
      .handle((resp, e) =>
        if (resp != null)
          resp.status match {
            case CommitResponse.CommitStatus.OK =>
              logger.info(s"Submission succeeded, correlationId=$submissionId")
              SubmissionResult.Acknowledged
            case error =>
              logger.error(s"Submission failed with an error, correlationId=$submissionId " +
                s"error='${error.toString}''")
              SubmissionResult.InternalError(error.toString)
          }
        else {
          logger.error(s"Submission failed with an exception, correlationId=$submissionId exception='${e.toString}'")
          e match {
            case grpc: StatusRuntimeException if grpc.getStatus.getCode == Status.Code.RESOURCE_EXHAUSTED =>
              SubmissionResult.Overloaded
            case _ =>
              SubmissionResult.InternalError(e.toString)
          }
        }
      )

  /** Submit a new configuration to the ledger. */
  override def submitConfiguration(
    maxRecordTime: Timestamp,
    submissionId: SubmissionId,
    config: Configuration
  ): CompletionStage[SubmissionResult] = {
    val submission =
      KeyValueSubmission.configurationToSubmission(maxRecordTime, submissionId, participantId, config)

    val commitReq = CommitRequest(
      submission = Envelope.enclose(submission),
      participantId = participantId.toString,
      correlationId = submissionId.toString
    )

    logger.info(s"Submit configuration, correlationId=$submissionId generation=${config.generation}")

    // FIXME(JM): Properly queue the transactions and execute in sequence from one place.
    submit(submissionId, commitReq)
  }

  override def submitTransaction(
    submitterInfo: SubmitterInfo,
    transactionMeta: TransactionMeta,
    transaction: SubmittedTransaction): CompletionStage[SubmissionResult] = {

    val submission: DamlKvutils.DamlSubmission =
      KeyValueSubmission.transactionToSubmission(
        submitterInfo,
        transactionMeta,
        transaction)

    val commandId = submission.getTransactionEntry.getSubmitterInfo.getCommandId

    val commitReq = CommitRequest(
      submission = Envelope.enclose(submission),
      participantId = participantId.toString,
      correlationId = commandId
    )

    logger.info(s"Submitting transaction, correlationId=$commandId")

    // FIXME(JM): Properly queue the transactions and execute in sequence from one place.
    submit(commandId, commitReq)
  }

  override def getLedgerInitialConditions(): Source[LedgerInitialConditions, NotUsed] =
    Source.single(
      LedgerInitialConditions(
        ledgerId = ledgerId,

        config = Configuration(
          generation = 0,
          timeModel = TimeModel(Duration.ofSeconds(1), Duration.ofSeconds(10), Duration.ofMinutes(2)).get
        ),

        // FIXME(JM): This in principle should be the record time of block 0,
        // or rather, the first value ever written to the time register(s).
        initialRecordTime = Time.Timestamp.Epoch
      )
    )

  override def currentHealth: HealthStatus = client.currentHealth

  override def stateUpdates(beginAfter: Option[Offset]): Source[(Offset, Update), NotUsed] = {
    val beginFromBlockId: Long =
      beginAfter
        .flatMap(_.components.headOption)
        .getOrElse(beginning)

    logger.info(s"Initializing thin replica source, offset=${beginFromBlockId}")

    trc.get
      .committedBlocks(beginFromBlockId)
      .flatMapConcat(processBlock)
      .filter {
        case (offset, _) =>
          if (beginAfter.isDefined)
            offset > beginAfter.get
          else true
      }
  }

  private def processBlock(block: TRClient.Block): Source[(Offset,Update),NotUsed] = {
    logger.trace(s"Processing block ${block.blockId}")

    try {
      val fragments =
        block.kvPairs
          .to[Array]
          .map { case Tuple2(prefixedKey, value) =>
            val fragmentKey =
              protobuf.LogEntryFragmentKey.parseFrom(prefixedKey.drop(1))
            val fragment = protobuf.LogEntryFragment.parseFrom(value)
            fragmentKey -> fragment
          }

      val updates =
        Defragmenter
          .fragmentsToUpdates(fragments)
          .zipWithIndex.map {
            case (update, idx) =>
              logger.info(s"Processing block, offset=${block.blockId}:$idx correlationId=${block.correlationId}")
              Offset(Array(block.blockId, idx.toLong)) -> update
          }

      Source(updates)
    } catch {
       case e: RuntimeException =>
         logger.error(s"Processing block failed with an exception, offset=${block.blockId} correlationId=${block.correlationId} error='${e}'")
         Source.failed(e)
     }
  }
}

object KVBCParticipantState{
  def apply (
      thisLedgerId: LedgerId, // Our ledger id.
      participantId: String,
      addresses: Array[String],
      useThinReplica: Boolean,
      maxFaultyReplicas: Short,
      )(implicit mat: Materializer, system: ActorSystem):KVBCParticipantState =
    new KVBCParticipantState(
        thisLedgerId,
        Ref.LedgerString.assertFromString(participantId),
        addresses,
        useThinReplica,
        maxFaultyReplicas)
}


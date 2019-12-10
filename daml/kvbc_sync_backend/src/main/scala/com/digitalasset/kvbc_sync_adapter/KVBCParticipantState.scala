// Copyright (c) 2019 Digital Asset (Switzerland) GmbH and/or its affiliates. All rights reserved.

package com.digitalasset.kvbc_sync_adapter

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
import com.digitalasset.kvbc.daml_data.ReadTransactionRequest
import com.digitalasset.kvbc.daml_events.CommittedTx
import com.digitalasset.ledger.api.health.{HealthStatus, Healthy}
import com.digitalasset.ledger.api.domain.PartyDetails

import com.daml.ledger.participant.state.v1._
import com.daml.ledger.participant.state.kvutils._
import org.slf4j.LoggerFactory

import scala.compat.java8.FutureConverters._
import scala.concurrent.ExecutionContext
import scala.util.{Failure, Success}

import java.util.concurrent.{CompletionStage, CompletableFuture, CompletionException}
import java.util.UUID
import io.grpc.{ConnectivityState, StatusRuntimeException, Status}

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
    host: String, // KVBC Server hostname
    port: Int,    // KVBC Server port number
  )(implicit val mat: Materializer,
    system: ActorSystem)
  extends ReadService
  with WriteService {

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
      participantId = participantId.toString
    )

    logger.info(s"Allocating party, party=$party submissionId=$submissionId")

    // FIXME(JM): Properly queue the transactions and execute in sequence from one place.
    submit(submissionId, commitReq)
  }


  private def generateRandomParty(): Ref.Party =
    Ref.Party.assertFromString(s"party-${UUID.randomUUID().toString.take(8)}")

  /** Upload DAML-LF packages to the ledger */
  override def uploadPackages(
      archives: List[Archive],
      sourceDescription: Option[String]): CompletionStage[UploadPackagesResult] = {

    //Since the gRPC layer below is synchronous, it is not necessary to memorize the submissionId
    val submissionId: String = UUID.randomUUID.toString

    val submission: DamlKvutils.DamlSubmission =
      KeyValueSubmission.archivesToSubmission(submissionId, archives, sourceDescription.getOrElse(""), participantId)

    val envelope = Envelope.enclose(submission)
    val commitReq = CommitRequest(
      submission = envelope,
      participantId = participantId.toString
    )

    logger.info(s"""Uploading package(s): packages=[${archives.map(_.getHash).mkString(",")}], submissionId=$submissionId, 
          |envelopeSize=${envelope.size}""".stripMargin.stripLineEnd)

    client
      .commitTransaction(commitReq)
      .toJava
      .toCompletableFuture
      .handle((resp, e) =>
        if (resp != null)
          resp.status match {
            case CommitResponse.CommitStatus.OK =>
              logger.info(s"Package upload completed successfully: submissionId=$submissionId")
              UploadPackagesResult.Ok

            case error =>
              //TODO: convert to SubmissionResult.InternalError, when provided
              //Unknown error is not a viable option. LedgerAPI client needs to get a meaningful error
              logger.error(s"Package upload failed with an error: submissionId=$submissionId, " +
                s"error=${error.toString}")
              UploadPackagesResult.InvalidPackage(s"Package upload failed with an error ${error.toString}")
        }
        else {
          logger.error(s"Package upload failed with an exception: submissionId=$submissionId, exception=${e.toString}")
          e match {
            case grpc: StatusRuntimeException if grpc.getStatus.getCode == Status.Code.RESOURCE_EXHAUSTED =>
              UploadPackagesResult.Overloaded
            case _ =>
              //TODO: convert to SubmissionResult.InternalError, when provided
              UploadPackagesResult.InternalError(e.toString)
          }
        }
      )
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
              logger.info(s"Submission succeeded: submissionId=$submissionId")
              SubmissionResult.Acknowledged
            case error =>
              logger.error(s"Submission failed with an error: submissionId=$submissionId, " +
                s"error=${error.toString}")
              SubmissionResult.InternalError(error.toString)
          }
        else {
          logger.error(s"Submission failed with an exception: submissionId=$submissionId, exception=${e.toString}")
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
      participantId = participantId.toString
    )

    logger.info(s"Submit configuration: submissionId=$submissionId, generation=${config.generation}")

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


    val commitReq = CommitRequest(
      submission = Envelope.enclose(submission),
      participantId = participantId.toString
    )

    val commandId = submission.getTransactionEntry.getSubmitterInfo.getCommandId

    logger.info(s"Submitting transaction: commandId=$commandId")

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

    client
      .committedTxs(beginFromBlockId)
      .flatMapConcat { committedTx =>
        logger.trace(s"Reading transaction ${committedTx.transactionId.toStringUtf8}...")

        readTransaction(committedTx)
          .filter {
            case (offset, _) =>
              if (beginAfter.isDefined)
                offset > beginAfter.get
              else true
          }
          .alsoTo(Sink.onComplete {
            case Success(Done) =>
              logger.info(s"Transaction read successfully: transactionId=${committedTx.transactionId.toStringUtf8}");
            case Failure(e) =>
              logger.info(s"Transaction read failed: transactionId=${committedTx.transactionId.toStringUtf8}, error=$e")
        })
      }
  }

  private def readTransaction(committedTx: CommittedTx): Source[(Offset,Update),NotUsed] = {
    Source.fromFuture(
      client
        .readKeys(
          ReadTransactionRequest(
            keys = List(committedTx.transactionId),
            blockId = committedTx.blockId
          ))
        .map { resp =>
          try {
            val logEntry =
              Envelope.open(resp.results.head.value) match {
                case Right(Envelope.LogEntryMessage(logEntry)) =>
                  logEntry
                case _ =>
                  sys.error(s"Envelope did not contain log entry")
              }

            KeyValueConsumption.logEntryToUpdate(
              DamlKvutils.DamlLogEntryId.newBuilder.setEntryId(committedTx.transactionId).build,
              logEntry
            ).zipWithIndex.map {
              case (update, idx) =>
                logger.trace(s"Processing transaction: transactionId=${committedTx.transactionId.toStringUtf8}, " +
                  s"offset=${committedTx.blockId}:$idx")
                Offset(Array(committedTx.blockId, idx.toLong)) -> update
            }
          } catch {
            //TODO: Stream breaks here, make sure index can deal with this
            case e: RuntimeException =>
              logger.error(s"Processing transaction failed with an exception: transactionId=${committedTx.transactionId.toStringUtf8}, " +
                s"error=${e.toString}")
              sys.error(e.toString)
          }
        })
      .mapConcat(identity) //Source[List[(Offset,Update)]] => Source[(Offset,Update)]
  }
}

object KVBCParticipantState{
  def apply (
      thisLedgerId: LedgerId, // Our ledger id.
      participantId: String,
      host: String, // KVBC Server hostname
      port: Int,    // KVBC Server port number
      )(implicit mat: Materializer, system: ActorSystem):KVBCParticipantState =
    new KVBCParticipantState(thisLedgerId, Ref.LedgerString.assertFromString(participantId), host, port)
}


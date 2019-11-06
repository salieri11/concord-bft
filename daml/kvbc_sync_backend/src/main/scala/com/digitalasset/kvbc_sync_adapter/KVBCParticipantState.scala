// Copyright (c) 2019 Digital Asset (Switzerland) GmbH and/or its affiliates. All rights reserved.

package com.digitalasset.kvbc_sync_adapter

import java.time.Duration

import akka.{Done, NotUsed}
import akka.actor.ActorSystem
import akka.stream.Materializer
import akka.stream.scaladsl._
import com.daml.ledger.participant.state.v1._
import com.digitalasset.daml.lf.data.{Ref, Time}
import com.digitalasset.daml_lf_dev.DamlLf.Archive
import com.digitalasset.grpc.adapter.AkkaExecutionSequencerPool
import com.digitalasset.kvbc.daml_commit._
import com.digitalasset.kvbc.daml_data.ReadTransactionRequest

import scala.util.{Failure, Success}
import com.daml.ledger.participant.state.kvutils._
import com.digitalasset.platform.common.util.DirectExecutionContext
import com.daml.ledger.participant.state.backport.TimeModel
import org.slf4j.LoggerFactory

import scala.collection.JavaConverters._
import scala.compat.java8.FutureConverters._
import scala.concurrent.ExecutionContext

import java.util.concurrent.CompletionStage
import java.util.concurrent.CompletableFuture
import java.util.UUID

import com.digitalasset.daml.lf.data.Ref.Party
import com.digitalasset.daml.lf.data.Time.Timestamp
import com.digitalasset.kvbc.daml_events.CommittedTx
import com.digitalasset.ledger.api.domain.PartyDetails
import io.grpc.ConnectivityState

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
    openWorld: Boolean
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
      hint: Option[String],
      displayName: Option[String]): CompletionStage[PartyAllocationResult] = {

    hint.map(p => Party.fromString(p)) match {
      case None =>
        allocatePartyOnLedger(generateRandomId(), displayName)
      case Some(Right(party)) =>
        allocatePartyOnLedger(party, displayName)
      case Some(Left(error)) =>
        CompletableFuture.completedFuture(PartyAllocationResult.InvalidName(error))
    }
  }

  private def allocatePartyOnLedger(
      party: String,
      displayName: Option[String]): CompletionStage[PartyAllocationResult] = {

    //Since the gRPC layer below is synchronous, it is not necessary to memorize the submissionId
    val submissionId: String = UUID.randomUUID.toString
    val submission: DamlKvutils.DamlSubmission =
      KeyValueSubmission.partyToSubmission(submissionId, Some(party), displayName, participantId)

    val commitReq = CommitRequest(
      submission = Envelope.enclose(submission),
      participantId = participantId.toString
    )

    logger.info(s"Allocating party: party=$party, submissionId: $submissionId")

    client
      .commitTransaction(commitReq)
      .toJava.toCompletableFuture
      .handle((resp, e) =>
        if (resp != null)
          resp.status match {
            case CommitResponse.CommitStatus.OK =>
              logger.info(s"Party successfully allocated: party=$party, submissionId=$submissionId")
              //TODO: Feed this response from the asynch channel
              PartyAllocationResult.Ok(PartyDetails(Ref.Party.assertFromString(party),displayName,true))
            case error =>
              //TODO: convert to SubmissionResult.InternalError, when provided
              logger.error(s"Party allocation failed with an error: party=$party, " +
                s"submissionId=$submissionId, error=${error.toString}")
              PartyAllocationResult.InvalidName(s"Party allocation failed with an error ${error.toString}")
          }
        else {
          //TODO: convert to SubmissionResult.InternalError, when provided
          logger.error(s"Party allocation failed with an exception: party=$party, submissionId=$submissionId, " +
            s"exception=${e.toString}")
          PartyAllocationResult.InvalidName(s"Party allocation failed with an exception ${e.toString}")}
      )
  }

  private def generateRandomId(): Ref.Party =
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
          //TODO: convert to SubmissionResult.InternalError, when provided
          logger.error(s"Package upload failed with an exception: submissionId=$submissionId, exception=${e.toString}")
          UploadPackagesResult.InvalidPackage(s"Package upload failed with an exception ${e.toString}")
        }
      )
  }

  /** Submit a new configuration to the ledger. */
  override def submitConfiguration(
    maxRecordTime: Timestamp,
    submissionId: String,
    config: Configuration
  ): CompletionStage[SubmissionResult] = {
    val submission =
      KeyValueSubmission.configurationToSubmission(maxRecordTime, submissionId, config)

    val commitReq = CommitRequest(
      submission = Envelope.enclose(submission),
      participantId = participantId.toString
    )

    logger.info(s"Submit configuration: submissionId=$submissionId, generation=${config.generation}")

    // FIXME(JM): Properly queue the transactions and execute in sequence from one place.
    client
      .commitTransaction(commitReq)
      .toJava
      .toCompletableFuture
      .handle((resp, e) =>
        if (resp != null)
          resp.status match {
            case CommitResponse.CommitStatus.OK =>
              logger.info(s"Configuration submission succeeded: submissionId=$submissionId")
              SubmissionResult.Acknowledged
            case error =>
              //TODO: convert to SubmissionResult.InternalError, when provided
              logger.error(s"Configuration submission failed with an error: submissionId=$submissionId, " +
                s"error=${error.toString}")
              SubmissionResult.NotSupported
          }
        else {
          logger.error(s"Configuration submission failed with an exception: submissionId=$submissionId, exception=${e.toString}")
          SubmissionResult.Overloaded
        }
      )
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
    client
      .commitTransaction(commitReq)
      .toJava
      .toCompletableFuture
      .handle((resp, e) =>
        if (resp != null)
          resp.status match {
            case CommitResponse.CommitStatus.OK =>
              logger.info(s"Transaction submission succeeded: commandId=$commandId")
              SubmissionResult.Acknowledged
            case _ =>
              //TODO: convert to SubmissionResult.InternalError, when provided
              logger.error(s"Transaction submission failed with unexpected commit response: commandId=$commandId")
              SubmissionResult.NotSupported
          }
        else {
          logger.error(s"Transaction submission failed with an exception: commandId=$commandId, exception=${e.toString}")
          SubmissionResult.Overloaded
        }
      )
  }

  override def getLedgerInitialConditions(): Source[LedgerInitialConditions, NotUsed] =
    Source.single(
      LedgerInitialConditions(
        ledgerId = ledgerId,

        config = Configuration(
          generation = 0,
          timeModel = TimeModel(Duration.ofSeconds(1), Duration.ofSeconds(10), Duration.ofMinutes(2)).get,
          authorizedParticipantId = Some(participantId),
          openWorld = openWorld
        ),

        // FIXME(JM): This in principle should be the record time of block 0,
        // or rather, the first value ever written to the time register(s).
        initialRecordTime = Time.Timestamp.Epoch
      )
    )



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
      client.readKeys(
        ReadTransactionRequest(
          keys = List(committedTx.transactionId),
          blockId = committedTx.blockId
        ))

        //TODO: Stream breaks here, make sure index can deal with this
        .recover {
          case e =>
            logger.error(s"Reading transaction keys failed with an exception: transactionId=${committedTx.transactionId.toStringUtf8}, " +
              s"error=${e.toString}")
            sys.error(e.toString)
        }
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
      openWorld: Boolean,
      )(implicit mat: Materializer, system: ActorSystem):KVBCParticipantState =
    new KVBCParticipantState(thisLedgerId, Ref.LedgerString.assertFromString(participantId), host, port, openWorld)
}


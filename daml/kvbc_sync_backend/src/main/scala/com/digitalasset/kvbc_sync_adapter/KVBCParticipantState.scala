// Copyright (c) 2019 Digital Asset (Switzerland) GmbH and/or its affiliates. All rights reserved.

package com.digitalasset.kvbc_sync_adapter

import java.time.Duration

import akka.{Done, NotUsed}
import akka.actor.ActorSystem
import akka.stream.Materializer
import akka.stream.scaladsl._
import com.daml.ledger.participant.state.v1._
import com.digitalasset.daml.lf.data.{Ref, Time}
import com.digitalasset.daml_lf.DamlLf.Archive
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
  val client = KVBCClient(host, port)

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

    val inputStateKeys =
      submission.getInputDamlStateList.asScala.map((k: DamlKvutils.DamlStateKey) => KVKey(KeyValueCommitting.packDamlStateKey(k)))

    val commitReq = CommitRequest(
      submission = Envelope.enclose(submission),
      inputState = inputStateKeys,
      participantId = participantId.toString
    )

    logger.info(s"Allocating party: party=$party, submissionId=$submissionId, inputStates=${inputStateKeys.size}")
    
    client
      .commitTransaction(commitReq)
      .toJava.toCompletableFuture
      .thenApply(resp =>
        resp.status match {
          case CommitResponse.CommitStatus.OK =>
            logger.info(s"Party successfully allocated: party=$party, submissionId=$submissionId")
            //TODO: Feed this response from the asynch channel
            PartyAllocationResult.Ok(PartyDetails(Ref.Party.assertFromString(party),displayName,true))
          case CommitResponse.CommitStatus.CONFLICT =>
            logger.error(s"Party allocation failed - already exists: party=$party, submissionId=$submissionId")
            PartyAllocationResult.AlreadyExists
            //TODO: Unknown error is not a possibility. LedgerAPI client needs to get a meaningful error
          case e =>
            //TODO: convert to PartyAllocationResult.InternalError, when provided
            //Unknown error is not a viable option. LedgerAPI client needs to get a meaningful error
            logger.error(s"Party allocation failed with an error $e")
            PartyAllocationResult.InvalidName(s"Party allocation failed with an error $e")
        })
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

    val inputStateKeys =
      submission.getInputDamlStateList.asScala.map((k: DamlKvutils.DamlStateKey) => KVKey(KeyValueCommitting.packDamlStateKey(k)))

    val envelope = Envelope.enclose(submission)
    val commitReq = CommitRequest(
      submission = envelope,
      inputState = inputStateKeys,
      participantId = participantId.toString
    )

    logger.info(s"""Uploading package(s): packages=[${archives.map(_.getHash).mkString(",")}], submissionId=$submissionId, 
          |envelopeSize=${envelope.size}, inputStates=${inputStateKeys.size}""".stripMargin.stripLineEnd)

    client
      .commitTransaction(commitReq)
      .toJava
      .toCompletableFuture
      .exceptionally( e => {
        //TODO: convert to UploadPackagesResult.InternalError, when provided
        logger.error(s"Package upload failed with an exception $e")
        sys.error(s"Package upload returned an exception $e")})
      .thenApply(resp =>
        resp.status match {

          case CommitResponse.CommitStatus.OK =>
            logger.info("Package upload completed successfully")
            UploadPackagesResult.Ok

          case CommitResponse.CommitStatus.CONFLICT =>
            //TODO: Conflict is not possible for a package upload, it should succeeed when two participants
            //deliver the same content
            logger.error("Package upload failed due to conflict")
            UploadPackagesResult.InvalidPackage("Package upload failed due to conflict")

          case e =>
            //TODO: convert to UploadPackagesResult.InternalError, when provided
            //Unknown error is not a viable option. LedgerAPI client needs to get a meaningful error
            logger.error(s"Package upload failed with an error $e")
            UploadPackagesResult.InvalidPackage(s"Package upload failed with an error $e")
        })
  }

  /** Submit a new configuration to the ledger. */
  override def submitConfiguration(
    maxRecordTime: Timestamp,
    submissionId: String,
    config: Configuration
  ): CompletionStage[SubmissionResult] = {
    val submission =
      KeyValueSubmission.configurationToSubmission(maxRecordTime, submissionId, config)

    val inputStateKeys =
      submission.getInputDamlStateList.asScala.map((k: DamlKvutils.DamlStateKey) => KVKey(KeyValueCommitting.packDamlStateKey(k)))

    val commitReq = CommitRequest(
      submission = Envelope.enclose(submission),
      inputState = inputStateKeys,
      participantId = participantId.toString
    )

    logger.info(s"""Submit configuration: submissionId=$submissionId, generation=${config.generation}
                   |inputStates=${inputStateKeys.size}""".stripMargin.stripLineEnd)

    // FIXME(JM): Properly queue the transactions and execute in sequence from one place.
    client
      .commitTransaction(commitReq)
      .map { resp =>
        resp.status match {
          case CommitResponse.CommitStatus.OK =>
            logger.info("Configuration submission succeeded")
          case CommitResponse.CommitStatus.CONFLICT =>
            // TODO(JM): Open architectural issue:
            // Command rejections should be handled in the state machine execution.
            logger.error("Configuration submission failed due to conflicting command")
          case _ =>
            logger.error("Configuration submission failed with unexpected commit response")
        }
      }(DirectExecutionContext)

    // FIXME(JM): Properly wrap the above commitTransaction into
    // CompletionStage.
    CompletableFuture.completedFuture({
      SubmissionResult.Acknowledged
    })
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


    val inputStateKeys =
      submission.getInputDamlStateList.asScala.map((k: DamlKvutils.DamlStateKey) => KVKey(KeyValueCommitting.packDamlStateKey(k)))

    val commitReq = CommitRequest(
      submission = Envelope.enclose(submission),
      inputState = inputStateKeys,
      participantId = participantId.toString
    )

    val commandId = submission.getTransactionEntry.getSubmitterInfo.getCommandId

    logger.info(s"Submitting transaction: commandId=$commandId, inputStates=${inputStateKeys.size}")

    // FIXME(JM): Properly queue the transactions and execute in sequence from one place.
    client
      .commitTransaction(commitReq)
      .map { resp =>
        resp.status match {
          case CommitResponse.CommitStatus.OK =>
            logger.info(s"Transaction submission succeeded: commandId=$commandId")
          case CommitResponse.CommitStatus.CONFLICT =>
            // TODO(JM): Open architectural issue:
            // Command rejections should be handled in the state machine execution.
            logger.error(s"Transaction submission failed due to conflicting command: commandId=$commandId")
          case _ =>
            logger.error(s"Transaction submission failed with unexpected commit response: commandId=$commandId")
        }
      }(DirectExecutionContext)

    // FIXME(JM): Properly wrap the above commitTransaction into
    // CompletionStage.
    CompletableFuture.completedFuture({
      SubmissionResult.Acknowledged
    })
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
        logger.trace(s"Reading transaction ${committedTx.transactionId.toString}...")

        readTransaction(committedTx)
          .filter {
            case (offset, _) =>
              if (beginAfter.isDefined)
                offset > beginAfter.get
              else true
          }
          .alsoTo(Sink.onComplete {
            case Success(Done) =>
              logger.info(s"Transaction read successfully: transactionId=${committedTx.transactionId.toString}");
            case Failure(e) =>
              logger.info(s"Transaction read failed: transactionId=${committedTx.transactionId.toString}, error=$e")
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
            logger.error(s"Reading transaction keys failed with an exception: transactionId=${committedTx.transactionId.toString}, " +
              s"error=$e")
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
                logger.trace(s"Processing transaction: transactionId=${committedTx.transactionId.toString}, " +
                  s"offset=${committedTx.blockId}:$idx")
                Offset(Array(committedTx.blockId, idx.toLong)) -> update
            }
          } catch {
            //TODO: Stream breaks here, make sure index can deal with this
            case e: RuntimeException =>
              logger.error(s"Processing transaction failed with an exception: transactionId=${committedTx.transactionId.toString}, " +
                s"error=$e")
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


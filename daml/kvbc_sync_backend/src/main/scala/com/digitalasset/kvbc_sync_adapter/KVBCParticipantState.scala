// Copyright (c) 2019 Digital Asset (Switzerland) GmbH and/or its affiliates. All rights reserved.

package com.digitalasset.kvbc_sync_adapter

import akka.{Done, NotUsed}
import akka.actor.ActorSystem
import akka.stream.Materializer
import akka.stream.scaladsl._
import com.daml.ledger.participant.state.v1._
import com.digitalasset.daml.lf.data.Time
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
import scala.concurrent.{ExecutionContext, Future}
import java.util.concurrent.CompletionStage
import java.util.concurrent.CompletableFuture

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

  def ledgerId = thisLedgerId

  private val logger = LoggerFactory.getLogger(this.getClass)
  val client = KVBCClient(host, port)

  def uploadArchive(archive: Archive): Future[Unit] = {
    logger.info(s"Uploading archive ${archive.getHash}...")
    val submission = KeyValueSubmission.packDamlSubmission(
      KeyValueSubmission.archivesToSubmission(List(archive), "example source description", "example participant id")
    )
    val commitReq = CommitRequest(submission)
    client
      .commitTransaction(commitReq)
      .map(resp =>
        resp.status match {
          case CommitResponse.CommitStatus.OK =>
            logger.info("Package successfully uploaded.")
          case CommitResponse.CommitStatus.CONFLICT =>
            sys.error("conflict on package upload!")
          case _ =>
            sys.error("unknown commitTransaction result")
        })
  }


  /** Allocate a party on the ledger */
  override def allocateParty(
      hint: Option[String],
      displayName: Option[String]): CompletionStage[PartyAllocationResult] =
    // TODO: Implement party management
    CompletableFuture.completedFuture(PartyAllocationResult.NotSupported)

  /** Upload a collection of DAML-LF packages to the ledger. */
  override def uploadPublicPackages(
      archives: List[Archive],
      sourceDescription: String): CompletionStage[SubmissionResult] =
    // TODO: Implement this, and remove [[uploadArchive]].
    CompletableFuture.completedFuture(SubmissionResult.NotSupported)

  override def submitTransaction(
    submitterInfo: SubmitterInfo,
    transactionMeta: TransactionMeta,
    transaction: SubmittedTransaction): CompletionStage[SubmissionResult] = {

    logger.info("submitTransaction: preparing...")
    val submission: DamlKvutils.DamlSubmission =
      KeyValueSubmission.transactionToSubmission(
        submitterInfo,
        transactionMeta,
        transaction)

    // NOTE(JM): Extract inputs from the submission so we do not have to deserialize it in the replica.
    val inputLogEntriesKeys =
      submission.getInputLogEntriesList.asScala.map((e: DamlKvutils.DamlLogEntryId) => KVKey(e.getEntryId))

    val inputStateKeys =
      submission.getInputDamlStateList.asScala.map((k: DamlKvutils.DamlStateKey) => KVKey(KeyValueCommitting.packDamlStateKey(k)))

    val commitReq = CommitRequest(
      submission = KeyValueSubmission.packDamlSubmission(submission),
      inputLogEntries = inputLogEntriesKeys,
      inputState = inputStateKeys
    )

    logger.info("submitTransaction: Committing transaction")

    // FIXME(JM): Properly queue the transactions and execute in sequence from one place.
    client
      .commitTransaction(commitReq)
      .foreach { resp =>
        resp.status match {
          case CommitResponse.CommitStatus.OK =>
            logger.info("Submitted successfully.")
          case CommitResponse.CommitStatus.CONFLICT =>
            // TODO(JM): Open architectural issue:
            // Command rejections should be handled in the state machine execution.
            sys.error("conflicting command")
          case _ =>
            sys.error("unknown commitTransaction result")
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
          timeModel = TimeModel.reasonableDefault
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
        .map(o => 1 + o)
        .getOrElse(0L)

    client
      .committedTxs(beginFromBlockId)
      .flatMapConcat { committedTx =>
        logger.info(s"Reading transaction ${committedTx.transactionId}...")

          Source.fromFuture(
            client.readKeys(
              ReadTransactionRequest(
                keys = List(committedTx.transactionId),
                blockId = committedTx.blockId
              )
            )
              .recover { case e =>
                logger.error("readKeys threw error: $e")
                throw e
              }
              .map { resp =>
                logger.info(s"Got ${resp.results.size} key-values back.")

                try {
                  val logEntry =
                    KeyValueConsumption.unpackDamlLogEntry(resp.results.head.value)
                  logger.info(s"Emitted offset ${committedTx.blockId}")
                  Offset(Array(committedTx.blockId)) ->
                    KeyValueConsumption.logEntryToUpdate(
                      DamlKvutils.DamlLogEntryId.newBuilder.setEntryId(committedTx.transactionId).build,
                      logEntry
                    )
                } catch {
                  case e: RuntimeException =>
                    logger.error(s"got error: $e")
                    sys.error(e.toString)
                }
              }
          ).alsoTo(Sink.onComplete {
            case Success(Done) =>
              logger.info("readKeys source completed successfully");
            case Failure(e) =>
              logger.info(s"readKeys failure: $e")

          })
      }
  }
}




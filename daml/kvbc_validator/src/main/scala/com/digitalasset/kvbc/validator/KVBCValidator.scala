// Copyright (c) 2019 Digital Asset (Switzerland) GmbH and/or its affiliates. All rights reserved.

package com.digitalasset.kvbc.validator

import java.time.{Duration, Instant}

import com.daml.ledger.participant.state.backport.TimeModel
import com.daml.ledger.participant.state.kvutils._
import com.daml.ledger.participant.state.v1.Configuration
import com.digitalasset.daml.lf.data.{Ref, Time}
import com.digitalasset.daml.lf.engine.Engine
import com.digitalasset.kvbc.daml_data._
import com.digitalasset.kvbc.daml_validator.ValidateResponse.Response
import com.digitalasset.kvbc.daml_validator._
import io.grpc.{Status, StatusRuntimeException}
import com.google.protobuf.ByteString
import org.slf4j.LoggerFactory
import com.github.blemale.scaffeine.{Cache, Scaffeine}

import scala.collection.JavaConverters._
import scala.concurrent.{ExecutionContext, Future}
import scala.concurrent.duration._

class KVBCValidator extends ValidationServiceGrpc.ValidationService {
  type ReplicaId = Long

  private val logger = LoggerFactory.getLogger(this.getClass)
  private val engine = Engine()

  // Use the global execution context. This uses threads proportional to
  // available processors.
  implicit val ec: ExecutionContext = ExecutionContext.global

  private val cache: Cache[(ReplicaId, DamlKvutils.DamlStateKey), DamlKvutils.DamlStateValue] =
    Scaffeine()
      .recordStats()
      .expireAfterWrite(1.hour)
      .maximumWeight(256*1024*1024) // ~256MB
      .weigher[(ReplicaId, DamlKvutils.DamlStateKey), DamlKvutils.DamlStateValue]{
        case (k: (ReplicaId, DamlKvutils.DamlStateKey), v: DamlKvutils.DamlStateValue) => v.getSerializedSize()
      }
      .build[(ReplicaId, DamlKvutils.DamlStateKey), DamlKvutils.DamlStateValue]()

  private val statsTimer = new java.util.Timer()
  private val statsTask = new java.util.TimerTask {
    def run() {
      logger.info(s"Cache stats: ${cache.stats()}")
    }
  }
  statsTimer.schedule(statsTask, 60*1000, 60*1000)

  // The default configuration to use if none has been uploaded.
  // FIXME(JM): Move into common place so getLedgerInitialConditions can use it as well.
  private val defaultConfig = Configuration(
    0,
    TimeModel(Duration.ofSeconds(1), Duration.ofSeconds(10), Duration.ofMinutes(2)).get,
    None,
    true)

  private def buildTimestamp(ts: Time.Timestamp): com.google.protobuf.timestamp.Timestamp = {
    val instant = ts.toInstant
    com.google.protobuf.timestamp.Timestamp(instant.getEpochSecond, instant.getNano)
  }

  private def parseTimestamp(ts: com.google.protobuf.timestamp.Timestamp): Time.Timestamp =
    Time.Timestamp.assertFromInstant(Instant.ofEpochSecond(ts.seconds, ts.nanos))

  def validateSubmission(request: ValidateRequest): Future[ValidateResponse] = Future {
    try {
      val replicaId = request.replicaId

      logger.trace(s"Validating submission: replicaId=$replicaId, participantId=${request.participantId}, entryId=${request.entryId.toStringUtf8}")

      // Unpack the submission.
      val submission =
        Envelope.open(request.submission) match {
          case Right(Envelope.SubmissionMessage(submission)) => submission
          case _ =>
            logger.error("Failed to parse submission")
            throw new StatusRuntimeException(
              Status.INVALID_ARGUMENT.withDescription("Unparseable submission")
            )
        }

      // Request missing inputs
      val allInputs: Set[DamlKvutils.DamlStateKey] = submission.getInputDamlStateList.asScala.toSet
      val providedInputs: Set[DamlKvutils.DamlStateKey] =
        request.inputState.map((kv: KeyValuePair) => KeyValueCommitting.unpackDamlStateKey(kv.key)).toSet
      def packageStateKey(packageId: Ref.PackageId): DamlKvutils.DamlStateKey =
        DamlKvutils.DamlStateKey.newBuilder.setPackageId(packageId).build
      val knownInputs: Map[DamlKvutils.DamlStateKey, Option[DamlKvutils.DamlStateValue]] =
        cache.getAllPresent(allInputs.map(replicaId -> _)).map { case ((_, key), v) => key -> Some(v) }
      val missingInputs =
        (allInputs -- knownInputs.keySet -- providedInputs)
          .map(KeyValueCommitting.packDamlStateKey)
          .toSeq

      if (missingInputs.nonEmpty) {
        logger.info(s"Requesting ${missingInputs.size} missing inputs...")
        ValidateResponse(
          Response.NeedState(NeedState(missingInputs))
        )
      } else {
        // Unpack the input state.
        val fetchedInputState: Map[DamlKvutils.DamlStateKey, Option[DamlKvutils.DamlStateValue]] =
          request.inputState.map { kv =>
            val key =
              KeyValueCommitting.unpackDamlStateKey(kv.key)

            key ->
              (if (kv.value.isEmpty)
                None
              else {
                Envelope.open(kv.value) match {
                  case Right(Envelope.StateValueMessage(v)) =>
                    cache.put(replicaId -> key, v)
                    Some(v)
                  case _ =>
                    logger.error(s"Corrupted state value of $key")
                    throw new StatusRuntimeException(
                      Status.INVALID_ARGUMENT.withDescription("Corrupted input state value")
                    )
                }
              })
          }
          .toMap
        val inputState = fetchedInputState ++ knownInputs

          val (logEntry, stateUpdates) = KeyValueCommitting.processSubmission(
            engine = engine,
            entryId = DamlKvutils.DamlLogEntryId.newBuilder.setEntryId(request.entryId).build,
            recordTime = parseTimestamp(request.recordTime.get),
            defaultConfig = defaultConfig,
            submission = submission,
            participantId = Ref.LedgerString.assertFromString(request.participantId),
            inputState = inputState)

          val outKeyPairs = stateUpdates
            .toArray
            .map { case (k, v) =>
              cache.put(replicaId -> k, v)
              KeyValuePair(
                KeyValueCommitting.packDamlStateKey(k),
                Envelope.enclose(v)
              )
            }
            // NOTE(JM): Since kvutils (still) uses 'Map' the results end up
            // in a non-deterministic order. Sort them to fix that.
            .sortBy(_.key.toByteArray.toIterable)

          val resp = ValidateResponse(
            Response.Result(
              Result(
                Envelope.enclose(logEntry),
              outKeyPairs)
            )
          )

          // FIXME(JM): REMOVE
          val logEntryHash = logEntry.toByteString.hashCode()
          val respHash = resp.toByteString.hashCode()

          logger.info(s"Submission validated, entryId=${request.entryId.toStringUtf8}, " +
            s"participantId=${request.participantId}, inputStates=${inputState.size}, stateUpdates=${stateUpdates.size}, " +
            s"resultPayload=${logEntry.getPayloadCase.toString}, " +
            s"logEntryHash=$logEntryHash, respHash=$respHash"
          )
          resp
      }
    } catch {
      case e: Throwable =>
        logger.error(s"Exception: $e")
        throw e
    }
  }
}

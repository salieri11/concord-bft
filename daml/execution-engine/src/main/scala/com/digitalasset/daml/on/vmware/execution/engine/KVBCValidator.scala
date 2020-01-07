// Copyright (c) 2019 Digital Asset (Switzerland) GmbH and/or its affiliates. All rights reserved.

package com.digitalasset.daml.on.vmware.execution.engine

import com.codahale.metrics.{MetricRegistry, Timer}
import com.daml.ledger.participant.state.kvutils.{DamlKvutils => KV, _}
import com.daml.ledger.participant.state.v1.{Configuration, ParticipantId, TimeModel}
import com.digitalasset.daml.lf.data.{Ref, Time}
import com.digitalasset.daml.lf.engine.Engine
import com.digitalasset.kvbc.daml_data._
import com.digitalasset.kvbc.daml_validator._
import com.digitalasset.ledger.api.health.{HealthStatus, Healthy, ReportsHealth}
import com.github.blemale.scaffeine.{Cache, Scaffeine}
import com.google.protobuf.ByteString
import io.grpc.{BindableService, ServerServiceDefinition, Status, StatusRuntimeException}
import java.time.{Duration, Instant}
import org.slf4j.LoggerFactory
import scala.collection.JavaConverters._
import scala.concurrent.duration._
import scala.concurrent.{ExecutionContext, Future}

class KVBCValidator(registry: MetricRegistry)(implicit ec: ExecutionContext)
    extends ValidationServiceGrpc.ValidationService
    with ReportsHealth
    with BindableService {

  private object Metrics {
    val prefix = "validator.service"

    val pendingSubmissions = registry.counter(s"$prefix.pending-submissions")
    val validateTimer = registry.timer(s"$prefix.validate-timer")
    val validatePendingTimer = registry.timer(s"$prefix.validate-pending-timer")
    val submissionSizes = registry.histogram(s"$prefix.submission-sizes")
    val outputSizes = registry.histogram(s"$prefix.output-sizes")
    val envelopeCloseTimer = registry.timer(s"$prefix.envelope-close-timer")
    val missingInputs = registry.histogram(s"$prefix.missing-inputs")
  }

  type ReplicaId = Long
  type RawEntryId = ByteString
  type StateInputs = Map[KV.DamlStateKey, Option[KV.DamlStateValue]]

  private val logger = LoggerFactory.getLogger(this.getClass)
  private val engine = Engine()

  private val cache: Cache[(ReplicaId, KV.DamlStateKey), KV.DamlStateValue] = {
    val cacheSizeEnv = System.getenv("KVBC_VALIDATOR_CACHE_SIZE")
    val cacheSize =
      if (cacheSizeEnv == null) {
        logger.warn("KVBC_VALIDATOR_CACHE_SIZE unset, defaulting to 'KVBC_VALIDATOR_CACHE_SIZE=256' (megabytes)")
        256 * 1024 * 1024
      } else {
        cacheSizeEnv.toInt
      }

    Scaffeine()
      .recordStats({ () => new KVBCMetricsStatsCounter(registry) })
      .expireAfterWrite(1.hour)
      .maximumWeight(cacheSize)
      .weigher[(ReplicaId, KV.DamlStateKey), KV.DamlStateValue]{
        case (k: (ReplicaId, KV.DamlStateKey), v: KV.DamlStateValue) =>
          k._2.getSerializedSize + v.getSerializedSize
      }
      .build[(ReplicaId, KV.DamlStateKey), KV.DamlStateValue]
  }

  case class PendingSubmission(
    participantId: ParticipantId,
    entryId: RawEntryId,
    recordTime: Time.Timestamp,
    submission: KV.DamlSubmission,
    inputState: Map[KV.DamlStateKey, Option[KV.DamlStateValue]]
  )

  // Pending submissions for each replica that require further state to process.
  // We only keep the last submission and assume that each replica executes each
  // request sequentially.
  private val pendingSubmissions =
    scala.collection.concurrent.TrieMap.empty[ReplicaId, PendingSubmission]

  // The default configuration to use if none has been uploaded.
  // FIXME(JM): Move into common place so getLedgerInitialConditions can use it as well.
  private val defaultConfig = Configuration(
    0,
    TimeModel(Duration.ofSeconds(1), Duration.ofSeconds(10), Duration.ofMinutes(2)).get)

  private def buildTimestamp(ts: Time.Timestamp): com.google.protobuf.timestamp.Timestamp = {
    val instant = ts.toInstant
    com.google.protobuf.timestamp.Timestamp(instant.getEpochSecond, instant.getNano)
  }

  private def parseTimestamp(ts: com.google.protobuf.timestamp.Timestamp): Time.Timestamp =
    Time.Timestamp.assertFromInstant(Instant.ofEpochSecond(ts.seconds, ts.nanos))

  def catchedTimedFutureThunk[A](timer: Timer)(act: => A): Future[A] =
    Future {
      val ctx = timer.time()
      try {
        act
      } catch {
        case e: Throwable =>
          logger.error(s"Submission validation failed, exception='$e'")
          throw e
      } finally {
        val _ = ctx.stop()
      }
    }

  def validatePendingSubmission(request: ValidatePendingSubmissionRequest): Future[ValidatePendingSubmissionResponse] = catchedTimedFutureThunk(Metrics.validatePendingTimer) {
    val replicaId = request.replicaId
    val correlationId = request.correlationId;

    logger.trace(s"Completing submission, replicaId=$replicaId correlationId=$correlationId")

    val pendingSubmission =
        pendingSubmissions
          .remove(replicaId)
          .getOrElse(sys.error(s"No pending submission for ${replicaId}, correlationId=$correlationId"))

    Metrics.pendingSubmissions.dec()

    if (pendingSubmission.entryId != request.entryId) {
      sys.error(s"Entry ids don't match, pending=${pendingSubmission.entryId} supplied=${request.entryId} correlationId=$correlationId")
    }

    val providedInputs: StateInputs =
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
                logger.error(s"Corrupted state value of $key, correlationId=$correlationId")
                throw new StatusRuntimeException(
                  Status.INVALID_ARGUMENT.withDescription("Corrupted input state value")
                )
            }
          })
      }
      .toMap

    val result = processPendingSubmission(
      replicaId,
      pendingSubmission.copy(inputState = pendingSubmission.inputState ++ providedInputs),
      correlationId
    )
    ValidatePendingSubmissionResponse(Some(result))
  }

  def validateSubmission(request: ValidateRequest): Future[ValidateResponse] = catchedTimedFutureThunk(Metrics.validateTimer) {
    val replicaId = request.replicaId
    val participantId = Ref.LedgerString.assertFromString(request.participantId)
    val correlationId = request.correlationId;

    logger.trace(s"Validating submission, replicaId=$replicaId participantId=${request.participantId} correlationId=$correlationId")

    Metrics.submissionSizes.update(request.submission.size())

    // Unpack the submission.
    val submission =
      Envelope.open(request.submission) match {
        case Right(Envelope.SubmissionMessage(submission)) => submission
        case _ =>
          logger.error("Failed to parse submission, correlationId=$correlationId")
          throw new StatusRuntimeException(
            Status.INVALID_ARGUMENT.withDescription("Unparseable submission")
          )
      }

    // Pull inputs from cache and create the pending submission
    val allInputs: Set[KV.DamlStateKey] = submission.getInputDamlStateList.asScala.toSet
    val cachedInputs: StateInputs =
      cache.getAllPresent(allInputs.map(replicaId -> _)).map { case ((_, key), v) => key -> Some(v) }
    val pendingSubmission = PendingSubmission(
      participantId,
      entryId = request.entryId,
      recordTime = parseTimestamp(request.recordTime.get),
      submission = submission,
      inputState = cachedInputs
    )

    // Check if some inputs are missing, and if so return with NeedState, otherwise
    // process the submission.
    val missingInputs =
      (allInputs -- cachedInputs.keySet)
        .map(KeyValueCommitting.packDamlStateKey)
        .toSeq
    Metrics.missingInputs.update(missingInputs.size)

    if (missingInputs.nonEmpty) {
      logger.trace(s"Requesting ${missingInputs.size} missing inputs, correlationId=$correlationId")
      pendingSubmissions(replicaId) = pendingSubmission
      Metrics.pendingSubmissions.inc()
      ValidateResponse(
        ValidateResponse.Response.NeedState(
          ValidateResponse.NeedState(missingInputs))
      )
    } else {
      ValidateResponse(
        ValidateResponse.Response.Result(
          processPendingSubmission(replicaId, pendingSubmission, correlationId)
        )
      )
    }
  }

  private def processPendingSubmission(replicaId: ReplicaId, pendingSubmission: PendingSubmission, correlationId: String): Result = {
    val submission = pendingSubmission.submission
    val allInputs: Set[KV.DamlStateKey] = submission.getInputDamlStateList.asScala.toSet
    assert((allInputs -- pendingSubmission.inputState.keySet).isEmpty)

    val (logEntry, stateUpdates) = KeyValueCommitting.processSubmission(
      engine = engine,
      entryId = KV.DamlLogEntryId.newBuilder.setEntryId(pendingSubmission.entryId).build,
      recordTime = pendingSubmission.recordTime,
      defaultConfig = defaultConfig,
      submission = submission,
      participantId = pendingSubmission.participantId,
      inputState = pendingSubmission.inputState)

    stateUpdates.foreach { case(k, v) => cache.put(replicaId -> k, v) }

    val result = Metrics.envelopeCloseTimer.time { () =>
      val outKeyPairs = stateUpdates
        .toArray
        .map { case (k, v) =>
          KeyValuePair(
            KeyValueCommitting.packDamlStateKey(k),
            Envelope.enclose(v)
          )
        }
        // NOTE(JM): Since kvutils (still) uses 'Map' the results end up
        // in a non-deterministic order. Sort them to fix that.
        .sortBy(_.key.toByteArray.toIterable)

      Result(
        Envelope.enclose(logEntry),
        outKeyPairs
      )
    }
    Metrics.outputSizes.update(result.logEntry.size())

    logger.info(s"Submission validated, correlationId=$correlationId participantId=${pendingSubmission.participantId} " +
      s"entryId=${pendingSubmission.entryId.toStringUtf8} inputStates=${pendingSubmission.inputState.size} stateUpdates=${stateUpdates.size} " +
      s"resultPayload=${logEntry.getPayloadCase.toString} recordTime=${pendingSubmission.recordTime.toString}")
    result
  }

  // A stateless component is always healthy
  def currentHealth(): HealthStatus = Healthy

  override def bindService(): ServerServiceDefinition =
    ValidationServiceGrpc.bindService(this, ec)
}

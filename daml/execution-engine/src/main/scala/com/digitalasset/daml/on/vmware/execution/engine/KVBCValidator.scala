// Copyright (c) 2019 Digital Asset (Switzerland) GmbH and/or its affiliates. All rights reserved.

package com.digitalasset.daml.on.vmware.execution.engine

import java.time.{Duration, Instant}

import akka.stream.Materializer
import com.codahale.metrics.{Counter, Histogram, Timer}
import com.daml.ledger.api.health.{HealthStatus, Healthy, ReportsHealth}
import com.daml.ledger.participant.state.kvutils.{Envelope, KeyValueCommitting, DamlKvutils => KV}
import com.daml.ledger.participant.state.pkvutils
import com.daml.ledger.participant.state.pkvutils.Fragmenter
import com.daml.ledger.participant.state.v1.{Configuration, ParticipantId, TimeModel}
import com.daml.ledger.validator.batch.{
  BatchedSubmissionValidator,
  BatchedSubmissionValidatorParameters,
  ConflictDetection
}
import com.daml.lf.data.{Ref, Time}
import com.daml.lf.engine.Engine
import com.daml.metrics.Metrics
import com.digitalasset.daml.on.vmware.common.Constants
import com.digitalasset.kvbc.daml_validator._
import com.github.blemale.scaffeine.{Cache, Scaffeine}
import com.google.protobuf.ByteString
import io.grpc.stub.StreamObserver
import io.grpc.{BindableService, ServerServiceDefinition, Status, StatusRuntimeException}
import org.slf4j.LoggerFactory

import scala.collection.JavaConverters._
import scala.concurrent.duration._
import scala.concurrent.{ExecutionContext, Future}

class KVBCValidator(metrics: Metrics)(implicit materializer: Materializer)
    extends ValidationServiceGrpc.ValidationService
    with ReportsHealth
    with BindableService {

  implicit val executionContext: ExecutionContext = materializer.executionContext

  private object Metrics {
    val prefix = "daml.validator.service"

    val pendingSubmissions: Counter = metrics.registry.counter(s"$prefix.pending-submissions")
    val validateTimer: Timer = metrics.registry.timer(s"$prefix.validate-timer")
    val validatePendingTimer: Timer = metrics.registry.timer(s"$prefix.validate-pending-timer")
    val submissionSizes: Histogram = metrics.registry.histogram(s"$prefix.submission-sizes")
    val outputSizes: Histogram = metrics.registry.histogram(s"$prefix.output-sizes")
    val envelopeCloseTimer: Timer = metrics.registry.timer(s"$prefix.envelope-close-timer")
    val missingInputs: Histogram = metrics.registry.histogram(s"$prefix.missing-inputs")
  }

  type ReplicaId = Long
  type RawEntryId = ByteString
  type StateInputs = Map[KV.DamlStateKey, Option[KV.DamlStateValue]]

  private val logger = LoggerFactory.getLogger(this.getClass)
  private val engine = Engine()
  private val keyValueCommitting = new KeyValueCommitting(engine, metrics)

  private lazy val cache: Cache[(ReplicaId, KV.DamlStateKey), KV.DamlStateValue] = {
    val cacheSize = StateCaches.determineCacheSize()
    Scaffeine()
      .recordStats({ () =>
        new KVBCMetricsStatsCounter(metrics.registry)
      })
      .expireAfterWrite(1.hour)
      .maximumWeight(cacheSize)
      .weigher[(ReplicaId, KV.DamlStateKey), KV.DamlStateValue] {
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
  private val defaultConfig = Configuration(
    generation = 0,
    timeModel = TimeModel.reasonableDefault,
    maxDeduplicationTime = Duration.ofDays(1) // Same default as SDK ledgers
  )

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

  override def validatePendingSubmission(
      request: ValidatePendingSubmissionRequest): Future[ValidatePendingSubmissionResponse] =
    catchedTimedFutureThunk(Metrics.validatePendingTimer) {
      val replicaId = request.replicaId
      val correlationId = request.correlationId

      logger.trace(s"Completing submission, replicaId=$replicaId correlationId=$correlationId")

      val pendingSubmission =
        pendingSubmissions
          .remove(replicaId)
          .getOrElse(
            sys.error(s"No pending submission for $replicaId, correlationId=$correlationId"))

      Metrics.pendingSubmissions.dec()

      if (pendingSubmission.entryId != request.entryId) {
        sys.error(
          s"Entry ids don't match, pending=${pendingSubmission.entryId} supplied=${request.entryId} correlationId=$correlationId")
      }

      val providedInputs: StateInputs =
        request.inputState.map { kv =>
          val key =
            keyValueCommitting.unpackDamlStateKey(kv.key.substring(1))

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
        }.toMap

      val result = processPendingSubmission(
        replicaId,
        pendingSubmission.copy(inputState = pendingSubmission.inputState ++ providedInputs),
        correlationId
      )
      ValidatePendingSubmissionResponse(Some(result))
    }

  override def validateSubmission(request: ValidateRequest): Future[ValidateResponse] =
    catchedTimedFutureThunk(Metrics.validateTimer) {
      val replicaId = request.replicaId
      val participantId = Ref.ParticipantId.assertFromString(request.participantId)
      val correlationId = request.correlationId

      logger.trace(
        s"Validating submission, replicaId=$replicaId participantId=${request.participantId} correlationId=$correlationId")

      Metrics.submissionSizes.update(request.submission.size())

      // Unpack the submission.
      val submission =
        Envelope.open(request.submission) match {
          case Right(Envelope.SubmissionMessage(submission)) => submission
          case _ =>
            logger.error(s"Failed to parse submission, correlationId=$correlationId")
            throw new StatusRuntimeException(
              Status.INVALID_ARGUMENT.withDescription("Unparseable submission")
            )
        }

      // Pull inputs from cache and create the pending submission
      val allInputs: Set[KV.DamlStateKey] = submission.getInputDamlStateList.asScala.toSet
      val cachedInputs: StateInputs =
        cache.getAllPresent(allInputs.map(replicaId -> _)).map {
          case ((_, key), v) => key -> Some(v)
        }
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
          .map(packStateKey)
          .toSeq
      Metrics.missingInputs.update(missingInputs.size)

      if (missingInputs.nonEmpty) {
        logger.info(
          s"Requesting missing inputs, size=${missingInputs.size} correlationId=$correlationId participantId=${pendingSubmission.participantId}")
        pendingSubmissions(replicaId) = pendingSubmission
        Metrics.pendingSubmissions.inc()
        ValidateResponse(
          ValidateResponse.Response.NeedState(ValidateResponse.NeedState(missingInputs))
        )
      } else {
        ValidateResponse(
          ValidateResponse.Response.Result(
            processPendingSubmission(replicaId, pendingSubmission, correlationId)
          )
        )
      }
    }

  private val validatorExecutionContext: ExecutionContext =
    ValidationServiceImpl.createInstrumentedExecutionContext(metrics.registry)

  private val readerCommitterFactoryFunction =
    PipelinedValidator.createReaderCommitter(() => StateCaches.createDefault(metrics.registry)) _

  private val batchValidator =
    BatchedSubmissionValidator[Unit](
      BatchedSubmissionValidatorParameters.default,
      keyValueCommitting,
      new ConflictDetection(metrics),
      metrics,
      engine)(executionContext = validatorExecutionContext)

  private val pipelinedValidator = new PipelinedValidator(
    batchValidator,
    readerCommitterFactoryFunction,
    new ConcordLedgerStateOperations.Metrics(metrics.registry)
  )(materializer = materializer, executionContext = validatorExecutionContext)

  override def validate(
      responseObserver: StreamObserver[EventFromValidator]): StreamObserver[EventToValidator] =
    pipelinedValidator.validateSubmissions(responseObserver)

  private def packStateKey(key: KV.DamlStateKey): ByteString =
    Constants.stateKeyPrefix.concat(keyValueCommitting.packDamlStateKey(key))

  private def packFragmentKey(key: pkvutils.protobuf.LogEntryFragmentKey): ByteString =
    Constants.fragmentKeyPrefix.concat(key.toByteString)

  private def processPendingSubmission(
      replicaId: ReplicaId,
      pendingSubmission: PendingSubmission,
      correlationId: String): Result = {
    val submission = pendingSubmission.submission
    val allInputs: Set[KV.DamlStateKey] = submission.getInputDamlStateList.asScala.toSet
    assert((allInputs -- pendingSubmission.inputState.keySet).isEmpty)
    val entryId = KV.DamlLogEntryId.newBuilder.setEntryId(pendingSubmission.entryId).build

    val (logEntry, stateUpdates) = keyValueCommitting.processSubmission(
      entryId = entryId,
      recordTime = pendingSubmission.recordTime,
      defaultConfig = defaultConfig,
      submission = submission,
      participantId = pendingSubmission.participantId,
      inputState = pendingSubmission.inputState,
    )

    stateUpdates.foreach { case (k, v) => cache.put(replicaId -> k, v) }

    val fragments = Fragmenter.logEntryToFragments(
      entryId,
      logEntry,
      pendingSubmission.participantId,
      pendingSubmission.inputState.collect {
        case (k, Some(v)) => k -> v
      },
      stateUpdates)

    val result: Result = Metrics.envelopeCloseTimer.time { () =>
      val outKeyPairs = stateUpdates.toArray
        .map {
          case (k, v) =>
            ProtectedKeyValuePair(
              packStateKey(k),
              Envelope.enclose(v),
              // TODO(JM): Reference constant for ID meaning only readable by this replica.
              Seq("dummy-replica-id"),
            )
        }
        // NOTE(JM): Since kvutils (still) uses 'Map' the results end up
        // in a non-deterministic order. Sort them to fix that.
        .sortBy(_.key.toByteArray.toIterable)

      Result(
        fragments.map { kv =>
          ProtectedKeyValuePair(
            packFragmentKey(kv.key),
            kv.value.toByteString,
            if (kv.acl.hasPublic) {
              Seq.empty
            } else {
              val ps = kv.acl.getProtected.getParticipantIdList.asScala
              if (ps.isEmpty) {
                Seq("dummy-replica-id")
              } else {
                ps
              }
            }
          )
        } ++ outKeyPairs,
        allInputs.toSeq.map(_.toByteString)
      )
    }
    Metrics.outputSizes.update(result.serializedSize)

    logger.info(s"Submission validated, correlationId=$correlationId participantId=${pendingSubmission.participantId} " +
      s"entryId=${pendingSubmission.entryId.toStringUtf8} inputStates=${pendingSubmission.inputState.size} stateUpdates=${stateUpdates.size} " +
      s"resultPayload=${logEntry.getPayloadCase.toString} recordTime=${pendingSubmission.recordTime.toString}")
    result
  }

  // A stateless component is always healthy
  def currentHealth(): HealthStatus = Healthy

  override def bindService(): ServerServiceDefinition =
    ValidationServiceGrpc.bindService(this, executionContext)
}

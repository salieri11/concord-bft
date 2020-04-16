// Copyright (c) 2019 Digital Asset (Switzerland) GmbH and/or its affiliates. All rights reserved.

package com.digitalasset.daml.on.vmware.execution.engine

import akka.stream.Materializer
import com.codahale.metrics.{MetricRegistry, Timer}
import com.daml.ledger.participant.state.kvutils.{Envelope, KeyValueCommitting, DamlKvutils => KV}
import com.daml.ledger.participant.state.pkvutils
import com.daml.ledger.participant.state.pkvutils.Fragmenter
import com.daml.ledger.participant.state.v1.{Configuration, ParticipantId, TimeModel}
import com.daml.ledger.validator.batch.{BatchValidator, BatchValidatorParameters, FragmentingLedgerOps, InternalBatchLedgerOps}
import com.daml.lf.data.{Ref, Time}
import com.daml.lf.engine.Engine
import com.digitalasset.daml.on.vmware.common.Constants
import com.digitalasset.kvbc.daml_validator._
import com.daml.ledger.api.health.{HealthStatus, Healthy, ReportsHealth}
import com.github.blemale.scaffeine.{Cache, Scaffeine}
import com.google.protobuf.ByteString
import io.grpc.{BindableService, ServerServiceDefinition, Status, StatusRuntimeException}
import io.grpc.stub.StreamObserver
import java.security.MessageDigest
import java.time.{Duration, Instant}
import org.slf4j.LoggerFactory

import scala.collection.JavaConverters._
import scala.collection.mutable
import scala.concurrent.duration._
import scala.concurrent.{ExecutionContext, Future}

class KVBCValidator(registry: MetricRegistry)(implicit materializer: Materializer)
    extends ValidationServiceGrpc.ValidationService
    with ReportsHealth
    with BindableService {

  implicit val executionContext: ExecutionContext = materializer.executionContext

  private object Metrics {
    val prefix = "daml.validator.service"

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
  private val keyValueCommitting = new KeyValueCommitting(registry)

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
    generation = 0,
    timeModel = TimeModel.reasonableDefault,
    maxDeduplicationTime = Duration.ofDays(1) // Same default as SDK ledgers
  )

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

  override def validatePendingSubmission(request: ValidatePendingSubmissionRequest): Future[ValidatePendingSubmissionResponse] = catchedTimedFutureThunk(Metrics.validatePendingTimer) {
    val replicaId = request.replicaId
    val correlationId = request.correlationId

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
      }
      .toMap

    val result = processPendingSubmission(
      replicaId,
      pendingSubmission.copy(inputState = pendingSubmission.inputState ++ providedInputs),
      correlationId
    )
    ValidatePendingSubmissionResponse(Some(result))
  }

  override def validateSubmission(request: ValidateRequest): Future[ValidateResponse] = catchedTimedFutureThunk(Metrics.validateTimer) {
    val replicaId = request.replicaId
    val participantId = Ref.ParticipantId.assertFromString(request.participantId)
    val correlationId = request.correlationId

    logger.trace(s"Validating submission, replicaId=$replicaId participantId=${request.participantId} correlationId=$correlationId")

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
        .map(packStateKey)
        .toSeq
    Metrics.missingInputs.update(missingInputs.size)

    if (missingInputs.nonEmpty) {
      logger.info(s"Requesting missing inputs, size=${missingInputs.size} correlationId=$correlationId participantId=${pendingSubmission.participantId}")
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

  val batchValidator =
    BatchValidator(
      params = BatchValidatorParameters.default,
      metricRegistry = registry
    )

  class KVBCCachingInternalBatchLedgerOps(replicaId: ReplicaId, ops: InternalBatchLedgerOps)(
    implicit executionContext: ExecutionContext)
    extends InternalBatchLedgerOps {

    private val readSet = mutable.Set.empty[KV.DamlStateKey]

    /** Return the set of state reads that have been performed.
     * Includes both cached and actual reads.
     * Useful when the read set is required for e.g. conflict detection. */
    def getReadSet(): Set[KV.DamlStateKey] =
      this.synchronized { readSet.toSet }

    override def readState(keys: Seq[KV.DamlStateKey]): Future[Seq[Option[KV.DamlStateValue]]] = {
      this.synchronized { readSet ++= keys }

      val cachedValues = cache.getAllPresent(keys.map(replicaId -> _)).map { case ((_, key), value) =>
          key -> Some(value)
      }
      val remainingKeys = keys.toSet -- cachedValues.keySet

      ops
        .readState(remainingKeys.toSeq)
        .map(remainingValues => remainingKeys.zip(remainingValues).toMap)
        .map { remaining =>
          remaining.collect {
            case (key, Some(value)) => cache.put(replicaId -> key, value)
          }
          val all: Map[KV.DamlStateKey, Option[KV.DamlStateValue]] = cachedValues ++ remaining
          keys.map(all(_))
        }
    }

    override def commit(
                         participantId: ParticipantId,
                         logEntryId: KV.DamlLogEntryId,
                         logEntry: KV.DamlLogEntry,
                         inputState: Map[KV.DamlStateKey, Option[KV.DamlStateValue]],
                         outputState: Map[KV.DamlStateKey,KV. DamlStateValue]): Future[Unit] = {
      cache.putAll(
        outputState.map { case (key, value) =>
          (replicaId, key) -> value
        }
      )
      ops.commit(participantId, logEntryId, logEntry, inputState, outputState)
    }
  }


  def validateBatch(responseObserver: StreamObserver[EventFromValidator]): StreamObserver[EventToValidator] = {
    implicit val executionContext = ExecutionContext.global

    val kvbcLedgerOps = new KVBCLedgerOps(responseObserver.onNext)
    new StreamObserver[EventToValidator] {
      override def onNext(value: EventToValidator): Unit = {
        value.toValidator match {
          case EventToValidator.ToValidator.ValidateRequest(request) =>
            logger.trace(s"Request received, correlationId=($request.correlationId} participantId=${request.participantId}")
            val ledgerOps = new KVBCCachingInternalBatchLedgerOps(request.replicaId, new FragmentingLedgerOps(kvbcLedgerOps))
            val recordTime = parseTimestamp(request.recordTime.get).toInstant
            batchValidator.validateAndCommit(
              recordTime,
              ParticipantId.assertFromString(request.participantId),
              request.correlationId,
              request.submission,
              ledgerOps
            ).foreach { _ =>
              val sortedReadSet =
                ledgerOps.getReadSet().map(_.toByteString).toSeq.sorted

              responseObserver.onNext(EventFromValidator().withDone(EventFromValidator.Done(sortedReadSet)))
              responseObserver.onCompleted()
              logger.info(s"Batch validation completed, correlationId=${request.correlationId} participantId=${request.participantId} recordTime=${recordTime.toString}")
            }
          case EventToValidator.ToValidator.ReadResult(result) =>
            kvbcLedgerOps.handleReadResult(result)

          case EventToValidator.ToValidator.Empty =>
            sys.error("Message EventToValidator is empty!")


        }
      }
      override def onError(t: Throwable): Unit =
        logger.error(s"validate() aborted due to an error: $t")
      override def onCompleted(): Unit =
        logger.trace("validate() completed")
    }
  }
  override def validate(responseObserver: StreamObserver[EventFromValidator]): StreamObserver[EventToValidator] =
    validateBatch(responseObserver)

  def validateSingle(responseObserver: StreamObserver[EventFromValidator]): StreamObserver[EventToValidator] = {
    // NOTE(JM): This adapts the pipelined interface into the existing one. This is temporary until
    // we bring in the batching validator which will properly use the pipelined interface.

    import EventFromValidator._
    def sendEvent(event: EventFromValidator => EventFromValidator): Unit =
      responseObserver.onNext(event(EventFromValidator()))

    // Hold the validate request to synthesize call to validatePendingSubmission
    // when a read completes.
    var optValidateRequest: Option[ValidateRequest] = None

    new StreamObserver[EventToValidator] {
      override def onNext(value: EventToValidator): Unit = {
        value.toValidator match {
          case EventToValidator.ToValidator.ValidateRequest(_) if optValidateRequest.isDefined =>
            sys.error("Already processing a submission")

          case EventToValidator.ToValidator.ValidateRequest(requestWithoutEntryId) =>
            // The new protocol does not provide an entry id from the caller side. We use
            // a sha256 hash of the submission as the entry id.
            val submissionBytes = requestWithoutEntryId.submission.toByteArray
            val entryId =
              MessageDigest.getInstance("SHA-256").digest(submissionBytes).map("%02x" format _).mkString
            val request = requestWithoutEntryId.withEntryId(ByteString.copyFromUtf8(entryId))
            optValidateRequest = Some(request)

            validateSubmission(request).foreach { response =>
              if (response.response.isNeedState) {
                // Send an event to fetch the state. We only send one event, hence we use static tag.
                sendEvent(_.withRead(Read(tag = "NEEDSTATE", keys = response.getNeedState.keys)))
              } else if (response.response.isResult) {
                val result = response.getResult
                sendEvent(_.withWrite(Write(result.updates)))
                sendEvent(_.withDone(Done(result.readSet)))
                responseObserver.onCompleted()
              } else {
                sys.error("ValidateResponse is empty")
              }
            }

          case EventToValidator.ToValidator.ReadResult(result) =>
            optValidateRequest match {
              case None =>
                sys.error("ReadResult received, but there is no request.")
              case Some(validateRequest) =>
                assert(result.tag == "NEEDSTATE")
                validatePendingSubmission(
                  ValidatePendingSubmissionRequest(
                    entryId = validateRequest.entryId,
                    replicaId = validateRequest.replicaId,
                    inputState = result.keyValuePairs,
                    correlationId = validateRequest.correlationId
                  )
                ).foreach { response =>
                  response.result
                    .fold(sys.error("validatePendingSubmission returned no result")) { result =>
                      sendEvent(_.withWrite(Write(result.updates)))
                      sendEvent(_.withDone(Done(result.readSet)))
                    }
                }
            }
        }
      }

      override def onError(t: Throwable): Unit =
        logger.error(s"validate() aborted due to an error: $t")

      override def onCompleted(): Unit =
        logger.trace("validate() completed")
    }
  }

  private def packStateKey(key: KV.DamlStateKey): ByteString =
    Constants.stateKeyPrefix.concat(keyValueCommitting.packDamlStateKey(key))

  private def packFragmentKey(key: pkvutils.protobuf.LogEntryFragmentKey): ByteString =
    Constants.fragmentKeyPrefix.concat(key.toByteString)

  private def processPendingSubmission(replicaId: ReplicaId, pendingSubmission: PendingSubmission, correlationId: String): Result = {
    val submission = pendingSubmission.submission
    val allInputs: Set[KV.DamlStateKey] = submission.getInputDamlStateList.asScala.toSet
    assert((allInputs -- pendingSubmission.inputState.keySet).isEmpty)
    val entryId = KV.DamlLogEntryId.newBuilder.setEntryId(pendingSubmission.entryId).build

    val (logEntry, stateUpdates) = keyValueCommitting.processSubmission(
      engine = engine,
      recordTime = pendingSubmission.recordTime,
      entryId = entryId,
      defaultConfig = defaultConfig,
      submission = submission,
      participantId = pendingSubmission.participantId,
      inputState = pendingSubmission.inputState)

    stateUpdates.foreach { case(k, v) => cache.put(replicaId -> k, v) }

    val fragments = Fragmenter.logEntryToFragments(
      entryId,
      logEntry,
      pendingSubmission.participantId,
      pendingSubmission.inputState.collect {
        case (k, Some(v)) => k -> v
      },
      stateUpdates)

    val result: Result = Metrics.envelopeCloseTimer.time { () =>
      val outKeyPairs = stateUpdates
        .toArray
        .map { case (k, v) =>
          ProtectedKeyValuePair(
            packStateKey(k),
            Envelope.enclose(v),
            Seq("dummy-replica-id"), // only readable by the replica
            // FIXME(JM): ^ what to put here?
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

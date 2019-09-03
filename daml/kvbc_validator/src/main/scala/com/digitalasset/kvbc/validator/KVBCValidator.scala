// Copyright (c) 2019 Digital Asset (Switzerland) GmbH and/or its affiliates. All rights reserved.

package com.digitalasset.kvbc.validator

import java.time.{Duration, Instant}

import com.digitalasset.kvbc.daml_validator._
import com.digitalasset.kvbc.daml_data._
import com.daml.ledger.participant.state.kvutils._
import com.daml.ledger.participant.state.v1.Configuration
import com.digitalasset.daml.lf.data.{Ref, Time}
import com.digitalasset.daml.lf.engine.Engine
import com.daml.ledger.participant.state.backport.TimeModel
import org.slf4j.LoggerFactory

import scala.collection.breakOut
import scala.concurrent.Future

class KVBCValidator extends ValidationServiceGrpc.ValidationService {
  private val logger = LoggerFactory.getLogger(this.getClass)
  private val engine = Engine()
  // FIXME(JM): Make part of request
  private val config = Configuration(
    0,
    TimeModel(Duration.ofSeconds(1), Duration.ofSeconds(1), Duration.ofMinutes(2)).get,
    None,
    true)

  private def buildTimestamp(ts: Time.Timestamp): com.google.protobuf.timestamp.Timestamp = {
    val instant = ts.toInstant
    com.google.protobuf.timestamp.Timestamp(instant.getEpochSecond, instant.getNano)
  }

  private def parseTimestamp(ts: com.google.protobuf.timestamp.Timestamp): Time.Timestamp =
    Time.Timestamp.assertFromInstant(Instant.ofEpochSecond(ts.seconds, ts.nanos))


  def validateSubmission(request: ValidateRequest): Future[ValidateResponse] = {
    // TODO(JM): Validation should happen in a thread pool the size of ~num of cores.

    logger.trace(s"Validating submission, request received: ${request.entryId.toStringUtf8}")

    val submission = KeyValueSubmission.unpackDamlSubmission(request.submission)

    val inputState =
      request.inputState.map { kv =>
        KeyValueCommitting.unpackDamlStateKey(kv.key) ->
          (if (kv.value.isEmpty)
            None
          else
            Some(KeyValueCommitting.unpackDamlStateValue(kv.value)))
      }

    val (logEntry, stateUpdates) = KeyValueCommitting.processSubmission(
      engine = engine,
      entryId = DamlKvutils.DamlLogEntryId.newBuilder.setEntryId(request.entryId).build,
      recordTime = parseTimestamp(request.recordTime.get),
      defaultConfig = config,
      submission = submission,
      participantId = Ref.LedgerString.assertFromString(request.participantId),
      inputState = inputState.toMap)

    logger.info(s"Submission validated, entryId: ${request.entryId.toStringUtf8}, " +
      s"participantId: ${request.participantId}, inputStates: ${inputState.size}, stateUpdates: ${stateUpdates.size}," +
      s"resultPayload: ${logEntry.getPayloadCase.toString}")

    Future.successful(
      ValidateResponse(
        logEntry = KeyValueCommitting.packDamlLogEntry(logEntry),
        stateUpdates.map { case (k, v) =>
          KeyValuePair(
            KeyValueCommitting.packDamlStateKey(k),
            KeyValueCommitting.packDamlStateValue(v)
          )
        }.toList
      )
    )
  }
}

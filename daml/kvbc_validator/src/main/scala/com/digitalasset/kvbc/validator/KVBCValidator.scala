// Copyright (c) 2019 Digital Asset (Switzerland) GmbH and/or its affiliates. All rights reserved.

package com.digitalasset.kvbc.validator

import java.time.Instant

import com.digitalasset.kvbc.kvbc_validator._
import com.digitalasset.kvbc.kvbc_data._
import com.daml.ledger.participant.state.kvutils._
import com.daml.ledger.participant.state.v1.Configuration
import com.digitalasset.daml.lf.data.Time
import com.digitalasset.daml.lf.engine.Engine
import com.digitalasset.platform.services.time.TimeModel
import org.slf4j.LoggerFactory

import scala.concurrent.Future

class KVBCValidator extends ValidationServiceGrpc.ValidationService {
  private val logger = LoggerFactory.getLogger(this.getClass)
  private val engine = Engine()
  // FIXME(JM): Make part of request
  private val config = Configuration(TimeModel.reasonableDefault)

  private def buildTimestamp(ts: Time.Timestamp): com.google.protobuf.timestamp.Timestamp = {
    val instant = ts.toInstant
    com.google.protobuf.timestamp.Timestamp(instant.getEpochSecond, instant.getNano)
  }

  private def parseTimestamp(ts: com.google.protobuf.timestamp.Timestamp): Time.Timestamp =
    Time.Timestamp.assertFromInstant(Instant.ofEpochSecond(ts.seconds, ts.nanos))


  def validateSubmission(request: ValidateRequest): Future[ValidateResponse] = {
    // TODO(JM): Validation should happen in a thread pool the size of ~num of cores.

    logger.info(s"Received validate request ${request.entryId.toStringUtf8}")

    val submission = KeyValueSubmission.unpackDamlSubmission(request.submission)

    val (logEntry, stateUpdates) = KeyValueCommitting.processSubmission(
      engine = engine,
      config = config,
      entryId = DamlKvutils.DamlLogEntryId.newBuilder.setEntryId(request.entryId).build,
      recordTime = parseTimestamp(request.recordTime.get),
      submission = submission,
      inputLogEntries =
        request.inputLogEntries.map { kv =>
          DamlKvutils.DamlLogEntryId.newBuilder.setEntryId(kv.key).build ->
            KeyValueCommitting.unpackDamlLogEntry(kv.value)
        }.toMap,
      inputState =
        request.inputState.map { kv =>
          KeyValueCommitting.unpackDamlStateKey(kv.key) ->
            (if (kv.value.isEmpty)
              None
            else
              Some(KeyValueCommitting.unpackDamlStateValue(kv.value)))
        }.toMap
    )

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

// Copyright (c) 2020 Digital Asset (Switzerland) GmbH and/or its affiliates. All rights reserved.

package com.digitalasset.daml.on.vmware.execution.engine.preexecution

import com.daml.ledger.participant.state.kvutils.{Err, Fingerprint}
import com.daml.ledger.validator.LedgerStateOperations.{Key, Value}
import com.daml.ledger.validator.preexecution.LedgerStateReaderWithFingerprints
import com.digitalasset.daml.on.vmware.execution.engine.ConcordLedgerStateReadHandler
import com.digitalasset.daml.on.vmware.execution.engine.metrics.ConcordLedgerStateOperationsMetrics
import com.digitalasset.kvbc.daml_validator.PreprocessorFromEngine.ReadRequest
import com.digitalasset.kvbc.daml_validator.PreprocessorToEngine.ReadResult
import com.digitalasset.kvbc.daml_validator.{KeyValueFingerprintTriple, PreprocessorFromEngine}
import com.google.protobuf.ByteString

import scala.concurrent.{ExecutionContext, Future, Promise}

final class ConcordPreExecutionLedgerStateReader(
    sendEvent: PreprocessorFromEngine => Unit,
    metrics: ConcordLedgerStateOperationsMetrics,
)(implicit val executionContext: ExecutionContext)
    extends LedgerStateReaderWithFingerprints
    with ConcordLedgerStateReadHandler[ReadResult, KeyValueFingerprintTriple] {
  import ConcordPreExecutionLedgerStateReader._

  override def read(keys: Seq[Key]): Future[Seq[(Option[Value], Fingerprint)]] = {
    // Create a promise that we'll fulfill from [[handleReadResult]] when the
    // result arrives.
    val promise = Promise[Seq[KeyValueFingerprintTriple]]()
    val readTag = getAndIncrementNextReadTag()
    recordPendingRead(readTag, promise, metrics.readCompletionTime.time())
    logger.trace(s"Sending read request: $readTag")
    sendEvent(
      PreprocessorFromEngine().withReadRequest(ReadRequest(tag = readTag.toString, keys = keys)))

    promise.future
      .map { keyValueFingerprintTriple =>
        logger.trace(
          s"Key-value-fingertip triple received for readTag=$readTag correlationId=$correlationId")
        val readResults = keyValueFingerprintTriple.map { triple =>
          triple.key -> (triple.value -> triple.fingerprint)
        }.toMap
        keys.map { key =>
          val optValueAndFingerprint = readResults.get(key)
          val resultValue = flattenOptionalByteString(optValueAndFingerprint.map(_._1))
          val resultFingerprint = flattenOptionalByteString(
            optValueAndFingerprint
              .map(_._2))
            .getOrElse(throw Err.DecodeError("Pre-execution", "Fingerprint missing"))
          recordCompleteHashIfTraceLog(key, resultValue)
          (resultValue, resultFingerprint)
        }
      }
  }

  override def extractTag(result: ReadResult): Int = result.tag.toInt

  override def extractReadSet(result: ReadResult): Seq[KeyValueFingerprintTriple] =
    result.keyValuePairs
}

object ConcordPreExecutionLedgerStateReader {
  private def flattenOptionalByteString(maybeByteString: Option[ByteString]) =
    maybeByteString.flatMap { value =>
      if (value.isEmpty) None else Some(value)
    }
}

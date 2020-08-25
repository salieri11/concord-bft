// Copyright (c) 2020 Digital Asset (Switzerland) GmbH and/or its affiliates. All rights reserved.

package com.digitalasset.daml.on.vmware.execution.engine.preexecution

import com.daml.ledger.participant.state.kvutils.Fingerprint
import com.daml.ledger.validator.LedgerStateOperations.{Key, Value}
import com.daml.ledger.validator.preexecution.LedgerStateReaderWithFingerprints
import com.digitalasset.daml.on.vmware.execution.engine.preexecution.StaticLedgerStateReader.KeysUnavailableException
import com.digitalasset.kvbc.daml_validator.PreExecuteRequest

import scala.concurrent.{ExecutionContext, Future}

private[preexecution] class StaticLedgerStateReader(
    val data: Map[Key, (Option[Value], Fingerprint)])(implicit executionContext: ExecutionContext)
    extends LedgerStateReaderWithFingerprints {
  override def read(keys: Seq[Key]): Future[Seq[(Option[Value], Fingerprint)]] = Future {
    val requestedKeySet = keys.toSet
    val unavailableKeys = requestedKeySet -- data.keySet
    if (unavailableKeys.nonEmpty) {
      throw KeysUnavailableException(unavailableKeys.toSeq)
    } else {
      keys.flatMap(data.get)
    }
  }
}

private[preexecution] object StaticLedgerStateReader {
  case class KeysUnavailableException(unavailableKeys: Seq[Key]) extends Exception {
    override def getMessage: String =
      s"Following keys are not available: ${unavailableKeys.mkString(", ")}"
  }

  def apply(readResult: PreExecuteRequest.ReadResult)(
      implicit executionContext: ExecutionContext): StaticLedgerStateReader = {
    val staticData = readResult.keyValuePairs.map { keyValuePair =>
      val value = (if (keyValuePair.value.isEmpty) {
                     None
                   } else {
                     Some(keyValuePair.value)
                   }) -> keyValuePair.fingerprint
      keyValuePair.key -> value
    }.toMap
    new StaticLedgerStateReader(staticData)
  }
}

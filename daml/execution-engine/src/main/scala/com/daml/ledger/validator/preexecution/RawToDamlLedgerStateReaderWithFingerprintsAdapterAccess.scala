// Copyright (c) 2020 Digital Asset (Switzerland) GmbH and/or its affiliates. All rights reserved.
// SPDX-License-Identifier: Apache-2.0

package com.daml.ledger.validator.preexecution

import com.daml.ledger.participant.state.kvutils.{DamlKvutils, Fingerprint}
import com.daml.ledger.validator.StateKeySerializationStrategy

import scala.concurrent.{ExecutionContext, Future}

// TODO Remove this hack once CachingDamlLedgerStateReaderWithFingerprints() is available in SDK

class RawToDamlLedgerStateReaderWithFingerprintsAdapterAccess(
    ledgerStateReaderWithFingerprints: LedgerStateReaderWithFingerprints,
    keySerializationStrategy: StateKeySerializationStrategy
)(implicit executionContext: ExecutionContext)
    extends DamlLedgerStateReaderWithFingerprints {
  private[this] val delegate = new RawToDamlLedgerStateReaderWithFingerprintsAdapter(
    ledgerStateReaderWithFingerprints,
    keySerializationStrategy)
  override def read(keys: Seq[DamlKvutils.DamlStateKey])
    : Future[Seq[(Option[DamlKvutils.DamlStateValue], Fingerprint)]] =
    delegate.read(keys)
}

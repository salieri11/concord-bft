// Copyright (c) 2020 Digital Asset (Switzerland) GmbH and/or its affiliates. All rights reserved.

package com.digitalasset.daml.on.vmware.common

import com.daml.ledger.participant.state.v1.ParticipantId

object Conversions {
  def toReplicaId(participantId: ParticipantId): String = participantId
}

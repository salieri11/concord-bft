package com.digitalasset.daml.on.vmware.write.service

import com.daml.ledger.participant.state.v1.ParticipantId
import com.daml.lf.data.Ref
import com.google.protobuf.ByteString

final case class CommitRequest(
    submission: ByteString,
    participantId: ParticipantId,
    correlationId: String,
    preExecute: Boolean = false)

object CommitRequest {
  def createEmpty(): CommitRequest =
    CommitRequest(
      submission = ByteString.EMPTY,
      participantId = Ref.ParticipantId.assertFromString("abcd"),
      correlationId = "")
}

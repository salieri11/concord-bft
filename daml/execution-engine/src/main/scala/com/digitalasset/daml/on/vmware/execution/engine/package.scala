package com.digitalasset.daml.on.vmware.execution

import com.daml.ledger.participant.state.pkvutils.{
  KeySerializationStrategy,
  PrefixingKeySerializationStrategy
}
import com.google.protobuf.ByteString

package object engine {

  val SharedKeySerializationStrategy: KeySerializationStrategy = PrefixingKeySerializationStrategy()

  /** Implicit sorting for ByteStrings for the execution engine package. */
  implicit val ordering: Ordering[ByteString] = {
    val comparator = ByteString.unsignedLexicographicalComparator()
    (x, y) =>
      comparator.compare(x, y)
  }
}

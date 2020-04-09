package com.digitalasset.daml.on.vmware.execution

import com.google.protobuf.ByteString

package object engine {


  /** Implicit sorting for ByteStrings for the execution engine package. */
  implicit val ordering: Ordering[ByteString] = {
    val comparator = ByteString.unsignedLexicographicalComparator()
    (x, y) => comparator.compare(x, y)
  }
}

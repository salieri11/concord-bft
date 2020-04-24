// Copyright (c) 2020 Digital Asset (Switzerland) GmbH and/or its affiliates. All rights reserved.

package com.digitalasset.daml.on.vmware.common
import com.google.protobuf.ByteString

/** Common constants. */
object Constants {

  /** The prefix to prepend to fragment keys.
    * This prefix is used in the read service to filter
    * for fragment key-value pairs.
    */
  val fragmentKeyPrefix: ByteString =
    ByteString.copyFromUtf8("F")

  /** The prefix to prepend to state keys. */
  val stateKeyPrefix: ByteString =
    ByteString.copyFromUtf8("S")
}

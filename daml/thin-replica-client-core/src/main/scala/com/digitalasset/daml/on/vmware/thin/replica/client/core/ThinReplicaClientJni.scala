// Copyright (c) 2020 Digital Asset (Switzerland) GmbH and/or its affiliates. All rights reserved.
package com.digitalasset.daml.on.vmware.thin.replica.client.core

import ch.jodersky.jni.nativeLoader
import scala.language.implicitConversions

// By adding this annotation, there is no need to call
// System.load("thin-replica-client-native0") before accessing native methods.
@nativeLoader("thin-replica-client-native0")
class ThinReplicaClientJni extends ThinReplicaClient {
  @native def initialize(
      clientId: String,
      maxFaulty: Short,
      privateKey: String,
      servers: Array[String],
      maxReadDataTimeout: Short,
      maxReadHashTimeout: Short,
      jaegerAgentHostPort: String): Boolean
  @native def subscribe(prefix: String): Boolean
  @native def subscribe(prefix: String, blockId: Long): Boolean
  @native def unsubscribe(): Boolean

  @native def pop(): Option[Update]
  @native def tryPop(): Option[Update]

  @native def acknowledgeBlockId(blockId: Long): Boolean

  @native def getTestUpdate: Option[Update]
}

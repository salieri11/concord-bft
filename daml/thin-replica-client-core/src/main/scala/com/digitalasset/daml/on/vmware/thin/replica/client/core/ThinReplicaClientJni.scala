// Copyright (c) 2020 Digital Asset (Switzerland) GmbH and/or its affiliates. All rights reserved.
package com.digitalasset.daml.on.vmware.thin.replica.client.core

import ch.jodersky.jni.nativeLoader

import scala.language.implicitConversions

// By adding this annotation, there is no need to call
// System.load("thin-replica-client-native0") before accessing native methods.
@nativeLoader("thin-replica-client-native0")
class ThinReplicaClientJni extends ThinReplicaClient {
  @native override def initialize(
      clientId: String,
      maxFaulty: Short,
      privateKey: String,
      servers: Array[String],
      maxReadDataTimeout: Short,
      maxReadHashTimeout: Short,
      jaegerAgentHostPort: String,
  ): Boolean

  @native override def currentHealth(): Int

  @native override def subscribe(prefix: String): Boolean

  @native override def subscribe(prefix: String, blockId: Long): Boolean

  @native override def unsubscribe(): Boolean

  @native override def pop(): Option[Update]

  @native override def tryPop(): Option[Update]

  @native override def acknowledgeBlockId(blockId: Long): Boolean

  @native override def getTestUpdate: Option[Update]
}

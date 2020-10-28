// Copyright (c) 2020 Digital Asset (Switzerland) GmbH and/or its affiliates. All rights reserved.
package com.digitalasset.daml.on.vmware.thin.replica.client.core

final case class Update(
    blockId: Long,
    kvPairs: Array[(Array[Byte], Array[Byte])],
    correlationId: String,
    spanContext: Array[Byte]
) {
  override def toString: String =
    this.getClass.getSimpleName +
      "(" + blockId.toString + ",[" +
      kvPairs.map(p => "(" + new String(p._1) + "," + new String(p._2) + ")").mkString(",") + "]," +
      correlationId + ")"
}

trait ThinReplicaClient {
  def initialize(
      clientId: String,
      maxFaulty: Short,
      privateKey: String,
      servers: Array[String],
      maxReadDataTimeout: Short,
      maxReadHashTimeout: Short,
      jaegerAgentHostPort: String,
  ): Boolean

  /**
    * @return 0 if healthy; 1 if unhealthy
    */
  def currentHealth(): Int

  def subscribe(prefix: String): Boolean

  def subscribe(prefix: String, blockId: Long): Boolean

  def unsubscribe(): Boolean

  def pop(): Option[Update]

  def tryPop(): Option[Update]

  def acknowledgeBlockId(blockId: Long): Boolean

  def getTestUpdate: Option[Update]
}

// Copyright (c) 2019 Digital Asset (Switzerland) GmbH and/or its affiliates. All rights reserved.
package com.digitalasset.daml.on.vmware.ledger.api.server

import java.io.File
import java.nio.file.Path

import com.daml.ledger.participant.state.v1.ParticipantId
import com.digitalasset.api.util.TimeProvider
import com.digitalasset.ledger.api.auth.{AuthService, AuthServiceWildcard}
import com.digitalasset.ledger.api.tls.TlsConfiguration
import com.digitalasset.platform.indexer.IndexerStartupMode

final case class Config(
    port: Int,
    portFile: Option[Path],
    archiveFiles: List[File],
    maxInboundMessageSize: Int,
    timeProvider: TimeProvider, // enables use of non-wall-clock time in tests
    jdbcUrl: String,
    tlsConfig: Option[TlsConfiguration],
    participantId: ParticipantId,
    startupMode: IndexerStartupMode,
    replicas: Seq[String],
    useThinReplica: Boolean,
    maxFaultyReplicas: Short,
    authService: Option[AuthService]
) {
  def withTlsConfig(modify: TlsConfiguration => TlsConfiguration): Config =
    copy(tlsConfig = Some(modify(tlsConfig.getOrElse(TlsConfiguration.Empty))))
}

object Config {
  // The default size of the buffer for gRPC message processing is 4MB 
  // Compare with: https://grpc.github.io/grpc-java/javadoc/io/grpc/ManagedChannelBuilder.html#maxInboundMessageSize-int-
  val DefaultMaxInboundMessageSize = 4194304

  def default: Config =
    new Config(
      port = 0,
      portFile = None,
      archiveFiles = List.empty,
      maxInboundMessageSize = DefaultMaxInboundMessageSize,
      timeProvider = TimeProvider.UTC,
      jdbcUrl = "",
      tlsConfig = None,
      participantId = ParticipantId.assertFromString("standalone-participant"),
      startupMode = IndexerStartupMode.MigrateAndStart,
      replicas = Seq("localhost:50051"),
      useThinReplica = false,
      maxFaultyReplicas = 1,
      authService = Some(AuthServiceWildcard)
    )
}

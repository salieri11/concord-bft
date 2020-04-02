package com.digitalasset.daml.on.vmware.ledger.api.server

import com.digitalasset.api.util.TimeProvider
import com.digitalasset.ledger.api.auth.{AuthService, AuthServiceWildcard}
import com.digitalasset.platform.indexer.IndexerStartupMode

final case class ExtraConfig(
    maxInboundMessageSize: Int,
    timeProvider: TimeProvider, // enables use of non-wall-clock time in tests
    startupMode: IndexerStartupMode,
    replicas: Seq[String],
    useThinReplica: Boolean,
    maxFaultyReplicas: Short,
    authService: Option[AuthService],
)

object ExtraConfig {
  val Default = ExtraConfig(
    maxInboundMessageSize = Config.DefaultMaxInboundMessageSize,
    timeProvider = TimeProvider.UTC,
    startupMode = IndexerStartupMode.MigrateAndStart,
    replicas = Seq("localhost:50051"),
    useThinReplica = false,
    maxFaultyReplicas = 1,
    authService = Some(AuthServiceWildcard)
  )
}

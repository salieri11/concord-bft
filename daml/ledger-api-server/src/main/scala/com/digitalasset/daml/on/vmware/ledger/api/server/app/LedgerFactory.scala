// Copyright (c) 2020 The DAML Authors. All rights reserved.
// SPDX-License-Identifier: Apache-2.0

package com.digitalasset.daml.on.vmware.ledger.api.server.app

import akka.stream.Materializer
import com.codahale.metrics.{MetricRegistry, SharedMetricRegistries}
import com.daml.ledger.participant.state.kvutils.api.KeyValueParticipantState
import com.daml.ledger.participant.state.kvutils.app.{Config, KeyValueLedger}
import com.daml.ledger.participant.state.v1.{ReadService, WriteService}
import com.digitalasset.ledger.api.auth.{AuthService, AuthServiceWildcard}
import com.digitalasset.logging.LoggingContext
import com.digitalasset.platform.apiserver.{ApiServerConfig, TimeServiceBackend}
import com.digitalasset.platform.configuration.CommandConfiguration
import com.digitalasset.platform.indexer.{IndexerConfig, IndexerStartupMode}
import com.digitalasset.resources.ResourceOwner
import scopt.OptionParser

import scala.concurrent.ExecutionContext

trait ConfigProvider[ExtraConfig] {
  val defaultExtraConfig: ExtraConfig

  def extraConfigParser(parser: OptionParser[Config[ExtraConfig]]): Unit

  def manipulateConfig(config: Config[ExtraConfig]): Config[ExtraConfig] =
    config

  def indexerConfig(config: Config[ExtraConfig]): IndexerConfig =
    IndexerConfig(
      config.participantId,
      jdbcUrl = config.serverJdbcUrl,
      startupMode = IndexerStartupMode.MigrateAndStart,
      allowExistingSchema = config.allowExistingSchemaForIndex,
    )

  def indexerMetricRegistry(config: Config[ExtraConfig]): MetricRegistry =
    SharedMetricRegistries.getOrCreate(s"indexer-${config.participantId}")

  def apiServerConfig(
      config: Config[ExtraConfig]): ApiServerConfig =
    ApiServerConfig(
      participantId = config.participantId,
      archiveFiles = config.archiveFiles.map(_.toFile).toList,
      port = config.port,
      address = config.address,
      jdbcUrl = config.serverJdbcUrl,
      tlsConfig = None,
      maxInboundMessageSize = Config.DefaultMaxInboundMessageSize,
      portFile = config.portFile,
    )

  def apiServerMetricRegistry(config: Config[ExtraConfig]): MetricRegistry =
    SharedMetricRegistries.getOrCreate(s"ledger-api-server-${config.participantId}")

  def commandConfig(config: Config[ExtraConfig]): CommandConfiguration =
    CommandConfiguration.default

  def timeServiceBackend(config: Config[ExtraConfig]): Option[TimeServiceBackend] = None

  def authService(config: Config[ExtraConfig]): AuthService =
    AuthServiceWildcard
}

trait ReadServiceOwner[+RS <: ReadService, ExtraConfig] extends ConfigProvider[ExtraConfig] {
  def readServiceOwner(config: Config[ExtraConfig])(
      implicit executionContext: ExecutionContext,
      materializer: Materializer,
      logCtx: LoggingContext,
  ): ResourceOwner[RS]
}

trait WriteServiceOwner[+WS <: WriteService, ExtraConfig] extends ConfigProvider[ExtraConfig] {
  def writeServiceOwner(config: Config[ExtraConfig])(
      implicit executionContext: ExecutionContext,
      materializer: Materializer,
      logCtx: LoggingContext,
  ): ResourceOwner[WS]
}

trait LedgerFactory[+RWS <: ReadWriteService, ExtraConfig]
    extends ReadServiceOwner[RWS, ExtraConfig]
    with WriteServiceOwner[RWS, ExtraConfig] {

  override final def readServiceOwner(
      config: Config[ExtraConfig])(
      implicit executionContext: ExecutionContext,
      materializer: Materializer,
      logCtx: LoggingContext): ResourceOwner[RWS] = readWriteServiceOwner(config)

  override final def writeServiceOwner(
      config: Config[ExtraConfig])(
      implicit executionContext: ExecutionContext,
      materializer: Materializer,
      logCtx: LoggingContext): ResourceOwner[RWS] = readWriteServiceOwner(config)

  def readWriteServiceOwner(config: Config[ExtraConfig])(
      implicit executionContext: ExecutionContext,
      materializer: Materializer,
      logCtx: LoggingContext,
  ): ResourceOwner[RWS]
}

object LedgerFactory {

  abstract class KeyValueLedgerFactory[KVL <: KeyValueLedger]
      extends LedgerFactory[KeyValueParticipantState, Unit] {
    override final val defaultExtraConfig: Unit = ()

    override final def readWriteServiceOwner(
        config: Config[Unit])(
        implicit executionContext: ExecutionContext,
        materializer: Materializer,
        logCtx: LoggingContext): ResourceOwner[KeyValueParticipantState] =
      for {
        readerWriter <- owner(config)
      } yield new KeyValueParticipantState(readerWriter, readerWriter)

    def owner(value: Config[Unit])(
        implicit executionContext: ExecutionContext,
        materializer: Materializer,
        logCtx: LoggingContext): ResourceOwner[KVL]

    override final def extraConfigParser(parser: OptionParser[Config[Unit]]): Unit =
      ()
  }
}

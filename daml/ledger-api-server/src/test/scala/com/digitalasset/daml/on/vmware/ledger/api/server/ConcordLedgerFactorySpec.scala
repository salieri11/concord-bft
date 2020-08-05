// Copyright (c) 2020 Digital Asset (Switzerland) GmbH and/or its affiliates. All rights reserved.

package com.digitalasset.daml.on.vmware.ledger.api.server

import com.codahale.metrics.MetricRegistry
import com.daml.ledger.api.testing.utils.AkkaBeforeAndAfterAll
import com.daml.ledger.participant.state.kvutils.api.{
  BatchingLedgerWriter,
  InterpretationCostBasedLedgerWriterChooser,
  LedgerWriter
}
import com.daml.ledger.participant.state.kvutils.app.Config
import com.daml.lf.data.Ref
import com.daml.logging.LoggingContext
import com.daml.metrics.Metrics
import com.digitalasset.daml.on.vmware.participant.state.ConcordLedgerWriter
import com.digitalasset.daml.on.vmware.write.service.ConcordWriteClient
import org.scalatest.{AsyncWordSpec, Matchers}
import org.scalatestplus.mockito.MockitoSugar

class ConcordLedgerFactorySpec
    extends AsyncWordSpec
    with AkkaBeforeAndAfterAll
    with Matchers
    with MockitoSugar {
  "createLedgerWriter" should {
    "create a writer chooser in case pre-execution threshold is specified" in {
      val config =
        Config.createDefault(ExtraConfig.Default.copy(preExecutionCostThreshold = Some(123L)))
      createInstance(config) shouldBe a[InterpretationCostBasedLedgerWriterChooser]
    }

    "create a batching ledger writer in case batching is enabled" in {
      val config = Config.createDefault(ExtraConfig.Default.copy(enableBatching = true))
      createInstance(config) shouldBe a[BatchingLedgerWriter]
    }

    "create a batching ledger writer in case batching is enabled and pre-execution threshold is specified" in {
      val config = Config.createDefault(
        ExtraConfig.Default.copy(enableBatching = true, preExecutionCostThreshold = Some(123L)))
      createInstance(config) shouldBe a[InterpretationCostBasedLedgerWriterChooser]
    }

    "create a ConcordLedgerWriter in case both batching and pre-execution are disabled" in {
      val config = Config.createDefault(
        ExtraConfig.Default.copy(enableBatching = false, preExecutionCostThreshold = None))
      createInstance(config) shouldBe a[ConcordLedgerWriter]
    }
  }

  private def createInstance(config: Config[ExtraConfig]): LedgerWriter = {
    LoggingContext.newLoggingContext { implicit logCtx =>
      ConcordLedgerFactory.createLedgerWriter(
        config,
        Ref.ParticipantId.assertFromString("test"),
        new Metrics(new MetricRegistry),
        mock[ConcordWriteClient])
    }
  }
}

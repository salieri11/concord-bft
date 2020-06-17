package com.digitalasset.daml.on.vmware.execution.engine.conflictdetection

import com.codahale.metrics.MetricRegistry
import com.daml.ledger.api.testing.utils.AkkaBeforeAndAfterAll
import com.daml.ledger.participant.state.kvutils.DamlKvutils.DamlLogEntry
import com.daml.ledger.validator.batch.ConflictDetection
import com.daml.logging.LoggingContext
import com.daml.metrics.Metrics
import org.scalatest.{AsyncWordSpec, Matchers}
import org.scalatestplus.mockito.MockitoSugar

class PatchedConflictDetectionSpec
    extends AsyncWordSpec
    with Matchers
    with MockitoSugar
    with AkkaBeforeAndAfterAll {
  "PatchedConflictDetection" should {
    "be pluggable as ConflictDetection" in {
      val damlMetrics = new Metrics(new MetricRegistry)
      val instance: ConflictDetection = new PatchedConflictDetection(damlMetrics)
      LoggingContext.newLoggingContext { implicit loggingContext =>
        instance.detectConflictsAndRecover(
          Set.empty,
          Map.empty,
          DamlLogEntry.getDefaultInstance,
          Map.empty) should not be None
      }
    }
  }
}

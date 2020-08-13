// Copyright (c) 2020 Digital Asset (Switzerland) GmbH and/or its affiliates. All rights reserved.

package com.digitalasset.daml.on.vmware.execution.engine.caching

import com.daml.caching.WeightedCache
import com.daml.ledger.participant.state.kvutils.DamlKvutils.{DamlStateKey, DamlStateValue}
import com.daml.ledger.participant.state.kvutils.Fingerprint
import com.daml.ledger.participant.state.kvutils.caching.`Message Weight`
import com.daml.ledger.validator.caching.CachingDamlLedgerStateReaderWithFingerprints.{
  StateCacheWithFingerprints,
  `Message-Fingerprint Pair Weight`
}
import com.digitalasset.daml.on.vmware.execution.engine.metrics.ValidatorCacheMetrics

private[engine] object PreExecutionStateCaches {
  def createDefault(metrics: ValidatorCacheMetrics): StateCacheWithFingerprints = {
    val cacheSize = StateCaches.determineCacheSize()
    metrics.stateValueCacheForPreExecution.synchronized {
      WeightedCache.from[DamlStateKey, (DamlStateValue, Fingerprint)](
        WeightedCache.Configuration(maximumWeight = cacheSize),
        metrics.stateValueCacheForPreExecution)
    }
  }
}

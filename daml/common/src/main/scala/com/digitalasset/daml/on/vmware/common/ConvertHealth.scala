package com.digitalasset.daml.on.vmware.common

import com.daml.ledger.api.health.HealthStatus

object ConvertHealth {
  def fromNative(nativeHealth: Int): HealthStatus =
    nativeHealth match {
      case 0 => HealthStatus.healthy
      case 1 => HealthStatus.unhealthy
      case _ => sys.error(s"Unknown health status: $nativeHealth")
    }
}

package com.digitalasset.daml.on.vmware.common

import com.daml.ledger.api.health.{HealthStatus, Healthy, Unhealthy}

object ConvertHealth {
  def fromNative(nativeHealth: Int): HealthStatus =
    nativeHealth match {
      case 0 => Healthy
      case 1 => Unhealthy
      case _ => sys.error(s"Unknown health status: $nativeHealth")
    }
}

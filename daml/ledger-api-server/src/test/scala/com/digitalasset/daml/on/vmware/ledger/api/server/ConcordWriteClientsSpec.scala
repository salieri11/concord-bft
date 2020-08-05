// Copyright (c) 2020 Digital Asset (Switzerland) GmbH and/or its affiliates. All rights reserved.

package com.digitalasset.daml.on.vmware.ledger.api.server

import org.scalatest.{Matchers, WordSpec}
import org.scalatestplus.mockito.MockitoSugar
import org.mockito.Mockito.{when, verify, times}
import ConcordWriteClients.waitForConcordWriteClientsToBeReady
import com.digitalasset.daml.on.vmware.write.service.ConcordWriteClient

class ConcordWriteClientsSpec extends WordSpec with Matchers with MockitoSugar {
  "waitForConcordWriteClientsToBeReady()" should {

    "wait for clientsToBeWaitedFor clients to be ready" in {
      val writeClient = mock[ConcordWriteClient]
      when(writeClient.ready).thenReturn(false, true)
      waitForConcordWriteClientsToBeReady(
        Seq(writeClient),
        clientsToBeWaitedFor = 1,
        sleepMillis = 1)
      verify(writeClient, times(2)).ready
    }

    "throw a RuntimeException if the number of attempts is exceeded" in {
      val writeClient = mock[ConcordWriteClient]
      when(writeClient.ready).thenReturn(false)
      a[RuntimeException] should be thrownBy waitForConcordWriteClientsToBeReady(
        Seq(writeClient),
        clientsToBeWaitedFor = 1,
        sleepMillis = 1,
        attempts = 1)
      verify(writeClient, times(1)).ready
    }

    "count ready clients once per ready count" in {
      val writeClient = mock[ConcordWriteClient]
      when(writeClient.ready).thenReturn(false, false, true, false, true, false, true, true)
      waitForConcordWriteClientsToBeReady(
        Seq(writeClient, writeClient),
        writeClientLabel = "secondary",
        clientsToBeWaitedFor = 2,
        sleepMillis = 1)
      verify(writeClient, times(8)).ready
    }
  }
}

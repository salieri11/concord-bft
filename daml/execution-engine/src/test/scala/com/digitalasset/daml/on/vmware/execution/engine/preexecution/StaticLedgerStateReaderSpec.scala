// Copyright (c) 2020 Digital Asset (Switzerland) GmbH and/or its affiliates. All rights reserved.

package com.digitalasset.daml.on.vmware.execution.engine.preexecution

import com.digitalasset.daml.on.vmware.execution.engine.preexecution.StaticLedgerStateReader.KeysUnavailableException
import com.digitalasset.kvbc.daml_validator.{KeyValueFingerprintTriple, PreExecuteRequest}
import com.google.protobuf.ByteString
import org.scalatest.{AsyncWordSpec, Matchers}
import org.scalatestplus.mockito.MockitoSugar

class StaticLedgerStateReaderSpec extends AsyncWordSpec with Matchers with MockitoSugar {
  "read" should {
    "throw exception in case a key is not available" in {
      val instance = new StaticLedgerStateReader(Map.empty)

      instance.read(Seq(aKey)).failed.map {
        case KeysUnavailableException(missingKeys) =>
          missingKeys should have length 1
          missingKeys should contain(aKey)
        case _ => fail
      }
    }

    "return successfully in case key is available" in {
      val expectedValue = Some(ByteString.copyFromUtf8("a value")) -> ByteString.EMPTY
      val instance = new StaticLedgerStateReader(Map(aKey -> expectedValue))

      instance.read(Seq(aKey)).map { actual =>
        actual should have size 1
        actual.head shouldBe expectedValue
      }
    }
  }

  "apply" should {
    "convert non-empty value as Some" in {
      val expectedValue = ByteString.copyFromUtf8("some")
      val input = PreExecuteRequest.ReadResult.of(
        Seq(
          KeyValueFingerprintTriple
            .of(key = aKey, value = expectedValue, fingerprint = aFingerprint)
        ))

      val instance = StaticLedgerStateReader(input)

      val actual = instance.data
      actual should have size 1
      actual should be definedAt aKey
      actual(aKey) shouldBe (Some(expectedValue) -> aFingerprint)
    }

    "convert empty value as None" in {
      val input = PreExecuteRequest.ReadResult.of(
        Seq(
          KeyValueFingerprintTriple
            .of(key = aKey, value = ByteString.EMPTY, fingerprint = aFingerprint)
        ))

      val instance = StaticLedgerStateReader(input)

      val actual = instance.data
      actual should have size 1
      actual should be definedAt aKey
      actual(aKey) shouldBe (None -> aFingerprint)
    }
  }

  private val aKey = ByteString.copyFromUtf8("a key")
  private val aFingerprint = ByteString.copyFromUtf8("0000")
}

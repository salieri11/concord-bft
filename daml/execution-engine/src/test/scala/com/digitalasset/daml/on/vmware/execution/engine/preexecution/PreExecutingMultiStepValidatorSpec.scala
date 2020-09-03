// Copyright (c) 2020 Digital Asset (Switzerland) GmbH and/or its affiliates. All rights reserved.

package com.digitalasset.daml.on.vmware.execution.engine.preexecution

import com.daml.caching.Cache
import com.daml.ledger.participant.state.pkvutils.KeySerializationStrategy
import com.daml.ledger.participant.state.v1.ParticipantId
import com.daml.ledger.validator.preexecution.{PreExecutingSubmissionValidator, PreExecutionOutput}
import com.daml.ledger.validator.privacy.LogFragmentsPreExecutingCommitStrategy.KeyValuePairsWithAccessControlList
import com.digitalasset.daml.on.vmware.execution.engine.preexecution.StaticLedgerStateReader.KeysUnavailableException
import com.digitalasset.daml.on.vmware.common.Conversions.toReplicaId
import com.digitalasset.kvbc.daml_validator.{
  PreExecuteRequest,
  PreExecuteResponse,
  WriteSet,
  PreExecutionOutput => ProtoPreExecutionOutput
}
import com.google.protobuf.ByteString
import com.vmware.concord.concord.{KeyAndFingerprint, PreExecutionResult, ReadSet}
import org.mockito.ArgumentMatchers.{any, anyString}
import org.mockito.Mockito.when
import org.scalatest.{AsyncWordSpec, Matchers}
import org.scalatestplus.mockito.MockitoSugar

import scala.concurrent.Future

class PreExecutingMultiStepValidatorSpec extends AsyncWordSpec with Matchers with MockitoSugar {
  "preExecute" should {
    "return read request in case key is not available" in {
      val expectedKey = ByteString.copyFromUtf8("a key")
      val mockValidator = mock[PreExecutingSubmissionValidator[KeyValuePairsWithAccessControlList]]
      when(
        mockValidator
          .validate(any(), anyString(), any(), any())(any())
      ).thenReturn(Future.failed(KeysUnavailableException(Seq(expectedKey))))
      val instance = new PreExecutingMultiStepValidator(
        mockValidator,
        () => Cache.none,
        aKeySerializationStrategy)

      instance
        .preExecute(PreExecuteRequest
          .of(preexecutionRequest = Some(anEmptyPreExecutionRequest), readResult = None))
        .map { actual =>
          actual shouldBe PreExecuteResponse().withReadRequest(
            PreExecuteResponse.ReadRequest.of(Seq(expectedKey)))
        }
    }

    "return result in case of successful validation" in {
      val mockValidator = mock[PreExecutingSubmissionValidator[KeyValuePairsWithAccessControlList]]
      when(
        mockValidator
          .validate(any(), anyString(), any(), any())(any())
      ).thenReturn(Future.successful(aPreExecutionOutput))
      val instance = new PreExecutingMultiStepValidator(
        mockValidator,
        () => Cache.none,
        aKeySerializationStrategy)

      instance
        .preExecute(PreExecuteRequest
          .of(preexecutionRequest = Some(anEmptyPreExecutionRequest), readResult = None))
        .map { actual =>
          actual shouldBe anEmptyPreExecuteResponse
        }
    }

    "convert read set from pre-execution output" in {
      val expectedReadSet = ReadSet.of(
        Seq(
          "a" -> "b",
          "c" -> "d"
        ).map {
          case (key, value) =>
            KeyAndFingerprint.of(ByteString.copyFromUtf8(key), ByteString.copyFromUtf8(value))
        })
      val mockValidator = mock[PreExecutingSubmissionValidator[KeyValuePairsWithAccessControlList]]
      val inputReadSet = expectedReadSet.keysWithFingerprints.map { keyAndValue =>
        keyAndValue.key -> keyAndValue.fingerprint
      }
      val aPreExecutionOutputWithReadSet = aPreExecutionOutput.copy(readSet = inputReadSet)
      when(
        mockValidator
          .validate(any(), anyString(), any(), any())(any())
      ).thenReturn(Future.successful(aPreExecutionOutputWithReadSet))
      val instance = new PreExecutingMultiStepValidator(
        mockValidator,
        () => Cache.none,
        aKeySerializationStrategy)

      instance
        .preExecute(PreExecuteRequest
          .of(preexecutionRequest = Some(anEmptyPreExecutionRequest), readResult = None))
        .map { actual =>
          actual.getPreexecutionResult.readSet shouldBe Some(expectedReadSet)
        }
    }

    "return a failed future in case of an exception during validation" in {
      val mockValidator = mock[PreExecutingSubmissionValidator[KeyValuePairsWithAccessControlList]]
      when(
        mockValidator
          .validate(any(), anyString(), any(), any())(any())
      ).thenReturn(Future.failed(new RuntimeException("something went wrong")))
      val instance = new PreExecutingMultiStepValidator(
        mockValidator,
        () => Cache.none,
        aKeySerializationStrategy)

      instance
        .preExecute(PreExecuteRequest
          .of(preexecutionRequest = Some(anEmptyPreExecutionRequest), readResult = None))
        .failed
        .map { actual =>
          actual shouldBe a[RuntimeException]
          actual.getMessage should equal("something went wrong")
        }
    }

    "fail in case no pre-execution request is passed" in {
      val instance = new PreExecutingMultiStepValidator(
        mock[PreExecutingSubmissionValidator[KeyValuePairsWithAccessControlList]],
        () => Cache.none,
        aKeySerializationStrategy)

      assertThrows[NoSuchElementException] {
        instance.preExecute(PreExecuteRequest.of(preexecutionRequest = None, readResult = None))
      }
    }
  }

  private def aKeySerializationStrategy = KeySerializationStrategy.createDefault()

  private val aParticipantId = "someParticipant"

  private val anEmptyPreExecutionRequest =
    PreExecuteRequest.PreExecutionRequest.of(
      ByteString.EMPTY,
      aParticipantId,
      0,
      "",
      ByteString.EMPTY)

  private def aPreExecutionOutput =
    new PreExecutionOutput[KeyValuePairsWithAccessControlList](
      None,
      None,
      Seq.empty,
      Seq.empty,
      Seq.empty,
      Set(ParticipantId.assertFromString(aParticipantId))
    )

  private val anEmptyProtoPreExecutionOutput =
    ProtoPreExecutionOutput.of(
      None,
      None,
      Some(WriteSet()),
      Some(WriteSet()),
      aPreExecutionOutput.involvedParticipants.map(toReplicaId).toSeq,
      aParticipantId
    )

  private val anEmptyPreExecuteResponse =
    PreExecuteResponse().withPreexecutionResult(
      PreExecutionResult.of(
        Some(ReadSet()),
        Some(anEmptyProtoPreExecutionOutput.toByteString),
        Some(""),
      ))
}

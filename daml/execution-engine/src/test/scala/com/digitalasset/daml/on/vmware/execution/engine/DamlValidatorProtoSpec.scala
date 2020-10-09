// Copyright (c) 2020 Digital Asset (Switzerland) GmbH and/or its affiliates. All rights reserved.

package com.digitalasset.daml.on.vmware.execution.engine

import java.nio.file.Paths

import org.scalatest.{Matchers, WordSpec}

import scala.io.Source.{fromFile, fromResource}

class DamlValidatorProtoSpec extends WordSpec with Matchers {
  DamlValidatorProto should {
    "be aligned with execution-engine-grpc's" in {
      val executionEngineGrpcProtoLines = fromResource(DamlValidatorProto).getLines()
      val vDamlProtoFile = fromFile(
        Paths
          .get(System.getProperty("user.dir"))
          .resolve(s"../concord/proto/$DamlValidatorProto")
          .toFile)
      try {
        val vDamlProtoLines = vDamlProtoFile.getLines()

        damlValidatorProtosAreAligned(
          executionEngineGrpcProtoLines.toVector,
          vDamlProtoLines.toVector) shouldBe Right()
      } finally {
        vDamlProtoFile.close()
      }
    }
  }

  private def damlValidatorProtosAreAligned(
      executionEngineDamlValidatorProtoLines: Vector[String],
      vDamlValidatorProtoLines: Vector[String]
  ): Either[String, Unit] = {
    var result: Either[String, Unit] = Right()
    var end = false
    var executionEngineProtoLinesIndex = 0
    var vDamlProtoLinesIndex = 0

    def advanceExecutionEngineProto(): Unit =
      executionEngineProtoLinesIndex += 1

    def advanceVDamlProto(): Unit =
      vDamlProtoLinesIndex += 1

    def advanceBoth(): Unit = {
      advanceExecutionEngineProto()
      advanceVDamlProto()
    }

    while (!end
      && executionEngineProtoLinesIndex < executionEngineDamlValidatorProtoLines.size
      && vDamlProtoLinesIndex < vDamlValidatorProtoLines.size) {
      val executionEngineProtoLine = executionEngineDamlValidatorProtoLines(
        executionEngineProtoLinesIndex).trim
      val vDamlProtoLine = vDamlValidatorProtoLines(vDamlProtoLinesIndex).trim

      if (executionEngineProtoLine == "// These messages have been inlined from concord.proto.") {
        end = true
      } else if (executionEngineProtoLine == "PreExecutionResult preexecution_result = 2;"
        && vDamlProtoLine == "com.vmware.concord.PreExecutionResult preexecution_result = 2;") {
        advanceBoth()
      } else if (executionEngineProtoLine.isEmpty || executionEngineProtoLine.startsWith("//")) {
        advanceExecutionEngineProto()
      } else if (vDamlProtoLine.isEmpty
        || vDamlProtoLine.startsWith("//")
        || vDamlProtoLine == "import \"concord.proto\";") {
        advanceVDamlProto()
      } else if (executionEngineProtoLine != vDamlProtoLine) {
        result = Left(
          "The following lines differ:\n" +
            s"> $executionEngineProtoLine\n" +
            s"< $vDamlProtoLine\n")
        end = true
      } else {
        advanceBoth()
      }
    }

    result
  }

  private lazy val DamlValidatorProto = "daml_validator.proto"
}

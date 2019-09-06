import Dependencies._

ThisBuild / scalaVersion     := "2.12.8"
ThisBuild / version          := "0.1.3-SNAPSHOT"
ThisBuild / organization     := "com.daml"
ThisBuild / organizationName := "Digital Asset, LLC"

lazy val akkaVersion = "2.5.13"
lazy val sdkVersion = "100.13.21"

lazy val protobuf = "com.google.protobuf" % "protobuf-java" % "3.2.0"
lazy val scalapb_runtime  = "com.thesamet.scalapb" %% "scalapb-runtime" % scalapb.compiler.Version.scalapbVersion % "protobuf"
lazy val scalapb_runtime_grpc = "com.thesamet.scalapb" %% "scalapb-runtime-grpc" % scalapb.compiler.Version.scalapbVersion

resolvers in Global +=
  "Digital Asset SDK" at "https://digitalassetsdk.bintray.com/DigitalAssetSDK"

lazy val commonSettings = Seq()

lazy val protos = (project in file("protos"))
  .settings(
    commonSettings,
    libraryDependencies ++=
      Seq(protobuf, scalapb_runtime, scalapb_runtime_grpc),
    PB.protoSources in Compile := Seq(
      target.value / "protobuf_external" / "protobuf",
      baseDirectory.value / "../../concord/proto",
    ),
    //excludeFilter in PB.generate := "*.proto",
    includeFilter in PB.generate := "daml*.proto",
    PB.targets in Compile := Seq(
      scalapb.gen() -> (sourceManaged in Compile).value
    ),
  )

lazy val kvbc_validator = (project in file("kvbc_validator"))
  .enablePlugins(JavaAppPackaging)
  .settings(
    commonSettings,
    name := "KVBC Validator",
    libraryDependencies ++= Seq(
      // DAML
      "com.digitalasset" %% "daml-lf-archive-scala" % sdkVersion,
      "com.digitalasset" % "daml-lf-archive" % sdkVersion,
      "com.digitalasset" %% "daml-lf-data" % sdkVersion,
      "com.digitalasset" %% "daml-lf-engine" % sdkVersion,
      "com.digitalasset" %% "daml-lf-language" % sdkVersion,

      "com.daml.ledger" %% "participant-state" % sdkVersion,
      "com.daml.ledger" %% "participant-state-kvutils" % sdkVersion,

      // Akka
      "com.typesafe.akka" %% "akka-stream" % akkaVersion,

      // Protobuf / grpc
      protobuf,

      // Logging
      "org.slf4j" % "slf4j-api" % "1.7.25",
      "ch.qos.logback" % "logback-core" % "1.2.3",
      "ch.qos.logback" % "logback-classic" % "1.2.3",
    ),
  )
  .dependsOn(protos)

lazy val kvbc_sync_backend = (project in file("kvbc_sync_backend"))
  .settings(
    commonSettings,
    name := "KVBC Participant State",
    libraryDependencies ++= Seq(
      // DAML
      "com.digitalasset" %% "daml-lf-archive-scala" % sdkVersion,
      "com.digitalasset" % "daml-lf-archive" % sdkVersion,
      "com.digitalasset" %% "daml-lf-data" % sdkVersion,
      "com.digitalasset" %% "daml-lf-engine" % sdkVersion,
      "com.digitalasset" %% "daml-lf-language" % sdkVersion,

      "com.daml.ledger" %% "participant-state" % sdkVersion,
      "com.daml.ledger" %% "participant-state-kvutils" % sdkVersion,

      // Akka
      "com.typesafe.akka" %% "akka-stream" % akkaVersion,

      // Protobuf / grpc
      protobuf,

      // Logging
      "org.slf4j" % "slf4j-api" % "1.7.25",
      "ch.qos.logback" % "logback-core" % "1.2.3",
      "ch.qos.logback" % "logback-classic" % "1.2.3",
    ),
  )
  .dependsOn(protos)


lazy val kvbc_ledger_server = (project in file("kvbc_ledger_server"))
  .enablePlugins(JavaAppPackaging)
  .settings(
    commonSettings,
    name := "KVBC Ledger Server",
    libraryDependencies ++= Seq(
      // DAML
      "com.digitalasset" %% "daml-lf-archive-scala" % sdkVersion,
      "com.digitalasset" % "daml-lf-archive" % sdkVersion,
      "com.digitalasset" %% "daml-lf-data" % sdkVersion,
      "com.digitalasset" %% "daml-lf-engine" % sdkVersion,
      "com.digitalasset" %% "daml-lf-language" % sdkVersion,

      "com.daml.ledger" %% "api-server-damlonx" % sdkVersion,
      "com.daml.ledger" %% "participant-state" % sdkVersion,
      "com.daml.ledger" %% "reference-participant-state-index" % sdkVersion,
      "com.daml.ledger" %% "participant-state-kvutils" % sdkVersion,
      "com.daml.ledger" %% "participant-state-index-v1" % sdkVersion,

      // Akka
      "com.typesafe.akka" %% "akka-stream" % akkaVersion,

      // Protobuf / grpc
      protobuf,

      // Logging
      "org.slf4j" % "slf4j-api" % "1.7.25",
      "ch.qos.logback" % "logback-core" % "1.2.3",
      "ch.qos.logback" % "logback-classic" % "1.2.3",
    ),
  )
  .dependsOn(protos, kvbc_sync_backend)

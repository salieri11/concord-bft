ThisBuild / scalaVersion     := "2.12.11"
ThisBuild / version          := "0.1.6-SNAPSHOT"
ThisBuild / organization     := "com.digitalasset"
ThisBuild / organizationName := "Digital Asset, LLC"
ThisBuild / coverageExcludedPackages := "com.digitalasset.kvbc.daml_commit.*;com.digitalasset.kvbc.daml_validator.*"

lazy val akkaVersion = "2.6.1"
lazy val sdkVersion = "1.6.0-snapshot.20201007.5314.0.b4a47d0b"
lazy val integrationKitVersion = "0.0.13-snapshot.20201008.891.0.1147d5a5"

lazy val protobuf = "com.google.protobuf" % "protobuf-java" % "3.8.0"
lazy val scalapb_runtime  = "com.thesamet.scalapb" %% "scalapb-runtime" % scalapb.compiler.Version.scalapbVersion % "protobuf"
lazy val scalapb_runtime_grpc = "com.thesamet.scalapb" %% "scalapb-runtime-grpc" % scalapb.compiler.Version.scalapbVersion

lazy val akka_stream = "com.typesafe.akka" %% "akka-stream" % akkaVersion

lazy val daml_lf_dev_archive_java_proto = "com.daml" % "daml-lf-dev-archive-java-proto" % sdkVersion
lazy val daml_lf_data = "com.daml" %% "daml-lf-data" % sdkVersion
lazy val daml_lf_engine = "com.daml" %% "daml-lf-engine" % sdkVersion
lazy val daml_lf_language = "com.daml" %% "daml-lf-language" % sdkVersion

lazy val participant_state = "com.daml" %% "participant-state" % sdkVersion

lazy val participant_state_kvutils = "com.daml" %% "participant-state-kvutils" % sdkVersion
lazy val participant_state_kvutils_app = "com.daml" %% "participant-state-kvutils-app" % sdkVersion
lazy val pkvutils = "com.daml.ledger.participant.state.pkvutils" % "pkvutils" % integrationKitVersion

lazy val slf4j_api = "org.slf4j" % "slf4j-api" % "1.7.26"
lazy val logback_core = "ch.qos.logback" % "logback-core" % "1.2.3"
lazy val logback_classic = "ch.qos.logback" % "logback-classic" % "1.2.3"

lazy val mockito_core = "org.mockito" % "mockito-core" % "2.24.0" % Test
lazy val scalatest = "org.scalatest" %% "scalatest" % "3.0.8" % Test
lazy val testing_utils = "com.daml" %% "testing-utils" % sdkVersion % Test

resolvers in Global += "Digital Asset" at "https://build-artifactory.eng.vmware.com/digitalasset.jfrog.io"

lazy val commonSettings = Seq()

lazy val damlAndConcordProtos = (project in file("protos"))
  .settings(
    commonSettings,
    libraryDependencies ++=
      Seq(protobuf, scalapb_runtime, scalapb_runtime_grpc),
    PB.protoSources in Compile := Seq(
      target.value / "protobuf_external" / "protobuf",
      baseDirectory.value / "../../concord/proto",
      baseDirectory.value / "../../communication/src/main/proto",
    ),
    includeFilter in PB.generate := "daml_commit.proto" || "concord.proto" || "concord_storage.proto",
    PB.targets in Compile := Seq(
      scalapb.gen() -> (sourceManaged in Compile).value
    ),
  )

lazy val common = (project in file("common"))
  .enablePlugins(JavaAppPackaging)
  .settings(
    commonSettings,
    name := "DAML on VMware Common",
    libraryDependencies ++= Seq(
      "com.daml" %% "build-info" % sdkVersion,
      pkvutils,
      participant_state,

      "io.dropwizard.metrics" % "metrics-core" % "4.1.2",
      "io.dropwizard.metrics" % "metrics-jvm" % "4.1.2",
      "io.dropwizard.metrics" % "metrics-servlets" % "4.1.2",
      "io.prometheus" % "simpleclient" % "0.8.1",
      "io.prometheus" % "simpleclient_dropwizard" % "0.8.1",
      "io.prometheus" % "simpleclient_servlet" % "0.8.1",

      "org.eclipse.jetty" % "jetty-servlets" % "9.4.20.v20190813",
      "org.eclipse.jetty" % "jetty-webapp" % "9.4.20.v20190813",

      slf4j_api,
      logback_classic,
      logback_core,

      protobuf,

      // Testing
      mockito_core,
      scalatest,
      testing_utils,
    ),
  )

lazy val execution_engine = (project in file("execution-engine"))
  .enablePlugins(JavaAppPackaging)
  .settings(
    commonSettings,
    name := "DAML on VMware Execution Engine",
    libraryDependencies ++= Seq(
      "com.digitalasset.daml.execution.engine" % "execution-engine" % integrationKitVersion,

      // Testing
      mockito_core,
      scalatest,
    ),
  )

lazy val write_service = (project in file("write-service"))
  .settings(
    commonSettings,
    name := "DAML on VMware Write Service",
    libraryDependencies ++= Seq(
      // DAML
      daml_lf_dev_archive_java_proto,
      daml_lf_data,
      daml_lf_engine,
      daml_lf_language,

      participant_state_kvutils,
      participant_state_kvutils_app,
      pkvutils,

      // Database support
      "org.postgresql" % "postgresql" % "42.2.9",

      // Akka
      akka_stream,

      // Protobuf / grpc
      protobuf,

      // Logging
      slf4j_api,
      logback_core,
      logback_classic,

      // Testing
      testing_utils,
      mockito_core,
      scalatest,
    ),
  )
  .dependsOn(damlAndConcordProtos, common, trc_core, bft_client_core)

lazy val ledger_api_server = (project in file("ledger-api-server"))
  .enablePlugins(JavaAppPackaging)
  .settings(
    commonSettings,
    name := "DAML on VMware Ledger API Server",
    libraryDependencies ++= Seq(
      // DAML
      daml_lf_dev_archive_java_proto,
      "com.daml" %% "contextualized-logging" % sdkVersion,

      daml_lf_data,
      daml_lf_engine,
      daml_lf_language,

      "com.daml" %% "sandbox" % sdkVersion,
      "com.daml" %% "ledger-api-auth" % sdkVersion,

      participant_state_kvutils,

      // Database support
      "org.postgresql" % "postgresql" % "42.2.9",

      // Akka
      akka_stream,

      // Protobuf / grpc
      protobuf,

      // Logging and monitoring
      slf4j_api,
      logback_core,
      logback_classic,

      // Testing
      scalatest,
      mockito_core,
      testing_utils,
      "org.scala-lang.modules" %% "scala-java8-compat" % "0.8.0" % Test,
    ),
  )
  .dependsOn(write_service, common, trc_core, bft_client_core, trc_native % Runtime, bft_client_native % Runtime)

lazy val trc_core = (project in file("thin-replica-client-core")) // regular scala code with @native methods
  .enablePlugins(JavaAppPackaging)
  .settings(
    target in javah := (sourceDirectory in nativeCompile in trc_native).value / "include",
    libraryDependencies ++= Seq(
      protobuf,
      participant_state,
      participant_state_kvutils
    )
  )
  .dependsOn(trc_native % Runtime)

lazy val trc_native = (project in file("thin-replica-client-native")) // native code and build script
  .settings(sourceDirectory in nativeCompile := sourceDirectory.value)
  .enablePlugins(JniNative)

lazy val bft_client_core = (project in file("bft-client-core")) // regular scala code with @native methods
  .enablePlugins(JavaAppPackaging)
  .settings(
    target in javah := (sourceDirectory in nativeCompile in bft_client_native).value / "include",
  )
  .dependsOn(bft_client_native % Runtime)

lazy val bft_client_native = (project in file("bft-client-native")) // native code and build script
  .settings(sourceDirectory in nativeCompile := sourceDirectory.value)
  .enablePlugins(JniNative)

lazy val printInfo = taskKey[Unit]("Prints env info")
printInfo := println(s"${(sourceDirectory in nativeCompile in trc_native).value.toString}")

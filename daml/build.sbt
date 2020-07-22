ThisBuild / scalaVersion     := "2.12.11"
ThisBuild / version          := "0.1.6-SNAPSHOT"
ThisBuild / organization     := "com.digitalasset"
ThisBuild / organizationName := "Digital Asset, LLC"
ThisBuild / coverageExcludedPackages := "com.digitalasset.kvbc.daml_commit.*;com.digitalasset.kvbc.daml_validator.*"

lazy val akkaVersion = "2.6.1"
lazy val sdkVersion = "1.4.0-snapshot.20200715.4733.0.d6e58626"
lazy val integrationKitVersion = "0.0.11-snapshot.20200717.676.0.32cde4a3"

lazy val protobuf = "com.google.protobuf" % "protobuf-java" % "3.8.0"
lazy val scalapb_runtime  = "com.thesamet.scalapb" %% "scalapb-runtime" % scalapb.compiler.Version.scalapbVersion % "protobuf"
lazy val scalapb_runtime_grpc = "com.thesamet.scalapb" %% "scalapb-runtime-grpc" % scalapb.compiler.Version.scalapbVersion

resolvers in Global ++=
  Seq(
    "Digital Asset KV OEM integration kit" at "https://build-artifactory.eng.vmware.com/digitalassetsdk.bintray.com/vmware-integration-kit",
  )

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
    includeFilter in PB.generate := "daml*.proto" || "concord.proto",
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
      "ch.qos.logback" % "logback-classic" % "1.2.3",
      "ch.qos.logback" % "logback-core" % "1.2.3",
      "com.daml" %% "build-info" % sdkVersion,
      "com.daml" %% "testing-utils" % sdkVersion % Test,
      "com.daml.ledger.participant.state.pkvutils" % "pkvutils" % integrationKitVersion,
      "io.dropwizard.metrics" % "metrics-core" % "4.1.2",
      "io.dropwizard.metrics" % "metrics-jvm" % "4.1.2",
      "io.dropwizard.metrics" % "metrics-servlets" % "4.1.2",
      "io.grpc" % "grpc-testing" % "1.29.0" % Test,
      "io.opentracing" % "opentracing-api" % "0.33.0",
      "io.opentracing" % "opentracing-mock" % "0.33.0",
      "io.opentracing" % "opentracing-util" % "0.33.0",
      "io.opentracing.contrib" % "opentracing-grpc" % "0.2.1",
      "io.opentracing.contrib" % "opentracing-tracerresolver" % "0.1.8",
      "io.prometheus" % "simpleclient" % "0.8.1",
      "io.prometheus" % "simpleclient_dropwizard" % "0.8.1",
      "io.prometheus" % "simpleclient_servlet" % "0.8.1",
      "org.eclipse.jetty" % "jetty-servlets" % "9.4.20.v20190813",
      "org.eclipse.jetty" % "jetty-webapp" % "9.4.20.v20190813",
      "org.mockito" % "mockito-core" % "2.24.0" % Test,
      "org.scalatest" %% "scalatest" % "3.0.8" % Test,
      "org.slf4j" % "slf4j-api" % "1.7.26",
      protobuf,
    ),
  )
  .dependsOn(damlAndConcordProtos)

lazy val execution_engine = (project in file("execution-engine"))
  .enablePlugins(JavaAppPackaging)
  .settings(
    commonSettings,
    name := "DAML on VMware Execution Engine",
    libraryDependencies ++= Seq(
      // DAML
      "com.daml" % "daml-lf-dev-archive-java-proto" % sdkVersion,
      "com.daml" %% "daml-lf-data" % sdkVersion,
      "com.daml" %% "daml-lf-engine" % sdkVersion,
      "com.daml" %% "daml-lf-language" % sdkVersion,

      "com.daml" %% "participant-state" % sdkVersion,
      "com.daml" %% "participant-state-kvutils" % sdkVersion,
      "com.daml.ledger.participant.state.pkvutils" % "pkvutils" % integrationKitVersion,
      "com.daml" %% "testing-utils" % sdkVersion % Test,
      "org.mockito" % "mockito-core" % "2.24.0" % Test,
      "org.scalatest" %% "scalatest" % "3.0.8" % Test,

      // Akka
      "com.typesafe.akka" %% "akka-stream" % akkaVersion,

      // Scaffaine, for state cache
      "com.github.blemale" %% "scaffeine" % "3.1.0",

      // Protobuf / grpc
      protobuf,
      "io.grpc" % "grpc-services" % "1.22.1",

      // Logging and monitoring
      "org.slf4j" % "slf4j-api" % "1.7.26",
      "ch.qos.logback" % "logback-core" % "1.2.3",
      "ch.qos.logback" % "logback-classic" % "1.2.3",
      "io.dropwizard.metrics" % "metrics-core" % "4.1.2",
      "io.dropwizard.metrics" % "metrics-jvm" % "4.1.2",
      "io.dropwizard.metrics" % "metrics-servlets" % "4.1.2",
      "io.prometheus" % "simpleclient" % "0.8.1",
      "io.prometheus" % "simpleclient_dropwizard" % "0.8.1",
      "io.prometheus" % "simpleclient_servlet" % "0.8.1",
      "org.eclipse.jetty"   %   "jetty-webapp"      % "9.4.20.v20190813",
      "org.eclipse.jetty"   %   "jetty-servlets"    % "9.4.20.v20190813",
    ),
  )
  .dependsOn(damlAndConcordProtos, common)

lazy val write_service = (project in file("write-service"))
  .settings(
    commonSettings,
    name := "DAML on VMware Write Service",
    libraryDependencies ++= Seq(
      // DAML
      "com.daml" % "daml-lf-dev-archive-java-proto" % sdkVersion,
      "com.daml" %% "daml-lf-data" % sdkVersion,
      "com.daml" %% "daml-lf-engine" % sdkVersion,
      "com.daml" %% "daml-lf-language" % sdkVersion,

      "com.daml" %% "participant-state" % sdkVersion,
      "com.daml" %% "participant-state-kvutils" % sdkVersion,
      "com.daml" %% "participant-state-kvutils-app" % sdkVersion,
      "com.daml.ledger.participant.state.pkvutils" % "pkvutils" % integrationKitVersion,

      "com.daml" %% "testing-utils" % sdkVersion % Test,
      "org.mockito" % "mockito-core" % "2.24.0" % Test,
      "org.scalatest" %% "scalatest" % "3.0.8" % Test,

      // Database support
      "org.postgresql" % "postgresql" % "42.2.9",

      // Akka
      "com.typesafe.akka" %% "akka-stream" % akkaVersion,

      // Protobuf / grpc
      protobuf,

      // Logging
      "org.slf4j" % "slf4j-api" % "1.7.26",
      "ch.qos.logback" % "logback-core" % "1.2.3",
      "ch.qos.logback" % "logback-classic" % "1.2.3",
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
      "com.daml" % "daml-lf-dev-archive-java-proto" % sdkVersion,
      "com.daml" %% "contextualized-logging" % sdkVersion,
      "com.daml" %% "daml-lf-data" % sdkVersion,
      "com.daml" %% "daml-lf-engine" % sdkVersion,
      "com.daml" %% "daml-lf-language" % sdkVersion,
      "com.daml" %% "sandbox" % sdkVersion,
      "com.daml" %% "ledger-api-auth" % sdkVersion,

      "com.daml" %% "participant-state" % sdkVersion,
      "com.daml" %% "participant-state-kvutils" % sdkVersion,

      // Database support
      "org.postgresql" % "postgresql" % "42.2.9",

      // Akka
      "com.typesafe.akka" %% "akka-stream" % akkaVersion,

      // Protobuf / grpc
      protobuf,

      // Logging and monitoring
      "org.slf4j" % "slf4j-api" % "1.7.26",
      "ch.qos.logback" % "logback-core" % "1.2.3",
      "ch.qos.logback" % "logback-classic" % "1.2.3",

      "org.scalatest" %% "scalatest" % "3.0.8" % Test,
      "org.mockito" % "mockito-core" % "2.24.0" % Test,
      "org.scala-lang.modules" %% "scala-java8-compat" % "0.8.0" % Test,
    ),
  )
  .dependsOn(write_service, common, trc_core, bft_client_core, trc_native % Runtime, bft_client_native % Runtime)

lazy val `replay-tool` = (project in file("execution-engine-replay-tool"))
  .enablePlugins(JavaAppPackaging)
  .settings(commonSettings)
  .dependsOn(common, execution_engine)

lazy val trc_core = (project in file("thin-replica-client-core")) // regular scala code with @native methods
  .enablePlugins(JavaAppPackaging)
  .settings(
    target in javah := (sourceDirectory in nativeCompile in trc_native).value / "include",
    libraryDependencies ++= Seq(
      protobuf,
      "com.daml" %% "participant-state" % sdkVersion,
      "com.daml" %% "participant-state-kvutils" % sdkVersion
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

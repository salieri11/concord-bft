

ThisBuild / scalaVersion     := "2.12.8"
ThisBuild / version          := "0.1.3-SNAPSHOT"
ThisBuild / organization     := "com.digitalasset"
ThisBuild / organizationName := "Digital Asset, LLC"

lazy val akkaVersion = "2.5.13"
lazy val sdkVersion = "100.13.42"

lazy val protobuf = "com.google.protobuf" % "protobuf-java" % "3.2.0"
lazy val scalapb_runtime  = "com.thesamet.scalapb" %% "scalapb-runtime" % scalapb.compiler.Version.scalapbVersion % "protobuf"
lazy val scalapb_runtime_grpc = "com.thesamet.scalapb" %% "scalapb-runtime-grpc" % scalapb.compiler.Version.scalapbVersion

resolvers in Global ++=
  Seq(
    "Digital Asset SDK" at "https://digitalassetsdk.bintray.com/DigitalAssetSDK",
    // "DA release" at "file:///tmp/repository"
  )

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

lazy val common = (project in file("common"))
  .enablePlugins(JavaAppPackaging)
  .settings(
    commonSettings,
    name := "DAML on VMware Common",
    libraryDependencies ++= Seq(
      
      // Logging and monitoring
      "org.slf4j" % "slf4j-api" % "1.7.25",
      "ch.qos.logback" % "logback-core" % "1.2.3",
      "ch.qos.logback" % "logback-classic" % "1.2.3",
      "io.dropwizard.metrics" % "metrics-core" % "4.0.0",
      "io.dropwizard.metrics" % "metrics-jvm" % "4.0.0",
      "io.dropwizard.metrics" % "metrics-servlets" % "4.0.0",
      "io.prometheus" % "simpleclient" % "0.8.0",
      "io.prometheus" % "simpleclient_dropwizard" % "0.8.0",
      "io.prometheus" % "simpleclient_servlet" % "0.8.0",
      "org.eclipse.jetty"   %   "jetty-webapp"      % "9.4.20.v20190813",
      "org.eclipse.jetty"   %   "jetty-servlets"    % "9.4.20.v20190813",
    ),
  )

lazy val execution_engine = (project in file("execution-engine"))
  .enablePlugins(JavaAppPackaging)
  .settings(
    commonSettings,
    name := "DAML on VMware Execution Engine",
    libraryDependencies ++= Seq(
      // DAML
      "com.digitalasset" % "daml-lf-dev-archive-java-proto" % sdkVersion,
      "com.digitalasset" %% "daml-lf-data" % sdkVersion,
      "com.digitalasset" %% "daml-lf-engine" % sdkVersion,
      "com.digitalasset" %% "daml-lf-language" % sdkVersion,
      
      "com.daml.ledger" %% "participant-state" % sdkVersion,
      "com.daml.ledger" %% "participant-state-kvutils" % sdkVersion,

      // Akka
      "com.typesafe.akka" %% "akka-stream" % akkaVersion,

      // Scaffaine, for state cache
      "com.github.blemale" %% "scaffeine" % "3.1.0",

      // Protobuf / grpc
      protobuf,
      "io.grpc" % "grpc-services" % "1.22.1",


      // Logging and monitoring
      "org.slf4j" % "slf4j-api" % "1.7.25",
      "ch.qos.logback" % "logback-core" % "1.2.3",
      "ch.qos.logback" % "logback-classic" % "1.2.3",
      "io.dropwizard.metrics" % "metrics-core" % "4.0.0",
      "io.dropwizard.metrics" % "metrics-jvm" % "4.0.0",
      "io.dropwizard.metrics" % "metrics-servlets" % "4.0.0",
      "io.prometheus" % "simpleclient" % "0.8.0",
      "io.prometheus" % "simpleclient_dropwizard" % "0.8.0",
      "io.prometheus" % "simpleclient_servlet" % "0.8.0",
      "org.eclipse.jetty"   %   "jetty-webapp"      % "9.4.20.v20190813",
      "org.eclipse.jetty"   %   "jetty-servlets"    % "9.4.20.v20190813",
    ),
  )
  .dependsOn(protos, common)

lazy val write_service = (project in file("write-service"))
  .settings(
    commonSettings,
    name := "DAML on VMware Write Service",
    libraryDependencies ++= Seq(
      // DAML
      "com.digitalasset" % "daml-lf-dev-archive-java-proto" % sdkVersion,
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


lazy val ledger_api_server = (project in file("ledger-api-server"))
  .enablePlugins(JavaAppPackaging)
  .settings(
    commonSettings,
    name := "DAML on VMware Ledger API Server",
    libraryDependencies ++= Seq(
      // DAML
      "com.digitalasset" % "daml-lf-dev-archive-java-proto" % sdkVersion,
      "com.digitalasset" %% "daml-lf-data" % sdkVersion,
      "com.digitalasset" %% "daml-lf-engine" % sdkVersion,
      "com.digitalasset" %% "daml-lf-language" % sdkVersion,
      "com.digitalasset.platform" %% "sandbox" % sdkVersion,
      "com.digitalasset.ledger" %% "ledger-api-auth" % sdkVersion,

      "com.daml.ledger" %% "participant-state" % sdkVersion,
      "com.daml.ledger" %% "participant-state-kvutils" % sdkVersion,

      // Akka
      "com.typesafe.akka" %% "akka-stream" % akkaVersion,

      // Protobuf / grpc
      protobuf,

      // Logging and monitoring
      "org.slf4j" % "slf4j-api" % "1.7.25",
      "ch.qos.logback" % "logback-core" % "1.2.3",
      "ch.qos.logback" % "logback-classic" % "1.2.3",

    ),
  )
  .dependsOn(protos, write_service, common)

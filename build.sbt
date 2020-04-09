import Dependencies._
import Profiles._
import com.typesafe.sbt.packager.docker.DockerPermissionStrategy

Global / onChangedBuildSource := IgnoreSourceChanges

val projectVersion = "0.3.1-SNAPSHOT"
val schemaVersion = "0.3.0-SNAPSHOT"

// Project-wide settings
ThisBuild / organization := "de.qaware.findfacts"
ThisBuild / version := projectVersion
ThisBuild / scalaVersion := "2.12.10"
ThisBuild / resolvers ++= Resolvers.all
// Use java 11
ThisBuild / javacOptions ++= Seq("-source", "11", "-target", "11")
// Parallel execution causes logging issues
ThisBuild / Test / parallelExecution := false
// Enable deprecation warnings
ThisBuild / scalacOptions ++= Seq("-deprecation", "-feature")
// Project-wide dependencies (intersection from all modules that can be run on their own)
ThisBuild / libraryDependencies ++= scalaTests ++ Seq(logging, wire, enum, files)
// Don't run tests in assembly
ThisBuild / assembly / test := {}

// Virtual sub-projects

// Root project aggregates all
lazy val root = (project in file("."))
  .settings(
    publish / skip := true,
    sonarProjects := Seq(
      `common-utils`,
      `common-da-api`,
      `common-da-solr`,
      `common-dt`,
      `importer-base`,
      `search-core`,
      `search-webapp`,
      `symbol-synonyms-tool`
    )
  )
  // Aggregate all modules
  .aggregate(
    `common-utils`,
    `common-da-api`,
    `common-da-solr`,
    `common-dt`,
    `importer-base`,
    `search-core`,
    loaders,
    ui,
    `symbol-synonyms-tool`
  )
  .enablePlugins(SonarConfiguration)

// Controls aggregation of sub-project that need the elm-compiler (depending if ui profile is active)
lazy val ui = project
  .settings(
    publish / skip := true,
    aggregate := active(UiProfile)
  )
  .aggregate(`search-webapp`)

// Controls aggregation of sub-projects that need Isabelle (depending if loaders profile is active)
lazy val loaders = project
  .settings(
    publish / skip := true,
    aggregate := active(LoaderProfile)
  )
  .aggregate(`isabelle`, `importer-isabelle`, memory)

// Controls aggregation of sub-projects with memory intensive tests (depending if memory profile is active)
lazy val memory = project
  .settings(
    publish / skip := true,
    aggregate := active(MemoryProfile)
  )
  .aggregate(`importer-it`)

// Real sub-projects

// Common utility
lazy val `common-utils` = project
  .settings(
    scapegoatIgnoredFiles += ".*scala.Using.scala",
    coverageExcludedFiles := "*.*scala.Using.scala",
    libraryDependencies += circeCore
  )
// Common api for data access
lazy val `common-da-api` = project
  .settings(libraryDependencies ++= Seq(shapeless, circeCore, circeGeneric % "test"))

// Common solr data access/bindings
lazy val `common-da-solr` = project
  .settings(
    libraryDependencies ++= Seq(circeCore, solr, classgraph, circeGeneric % "test") ++ loggingBackend.map(_ % "test")
  )
  .dependsOn(`common-da-api`, `common-utils`)

// Common business logic data types
lazy val `common-dt` = project
  .configs(IntegrationTest)
  .settings(
    Defaults.itSettings,
    libraryDependencies ++= circe ++ Seq(shapeless, scalaTest % "it") ++ loggingBackend.map(_ % "it")
  )
  .dependsOn(`common-da-api`, `common-utils`, `common-da-solr` % "it")

// Importer for isabelle dumps
lazy val `importer-base` = project
  .configs(IntegrationTest)
  .settings(
    libraryDependencies ++= Seq(cmdOpts, cats)
  )
  .dependsOn(`common-dt`, `common-da-solr`, `common-utils`)

// Importer

// Isabelle project dependency
lazy val isabelle = project
  .settings(
    publish / skip := true,
    unmanagedJars in Compile ++= (baseDirectory.value / "lib" / "classes" ** "*.jar").get(),
    libraryDependencies ++= isabelleDependencies,
    isabelleExecutable := baseDirectory.value / "bin" / "isabelle",
    isabelleCommand := "dump"
  )
  .enablePlugins(IsabellePlugin)

// Importer Isabelle projects. Follows Isabelle conventions.
lazy val `importer-isabelle` = project
  .settings(
    publish / skip := true,
    isabelleCommand := "dump_importer",
    isabelleExecutable := (baseDirectory in isabelle).value / "bin" / "isabelle",
    isabelleSettings := Seq("SOLR_CONFIGSET=theorydata-" + schemaVersion),
    libraryDependencies ++= loggingBackend
  )
  .dependsOn(`importer-base`, `isabelle`)
  .enablePlugins(IsabelleToolPlugin)

// Integration test to check integration between Isabelle dump and dump_importer
val runImport = settingKey[Boolean]("Flag to set if import should be run before tests")
lazy val `importer-it` = project
  .configs(IntegrationTest)
  .settings(
    publish / skip := true,
    Defaults.itSettings,
    runImport := true,
    test in IntegrationTest := Def.taskDyn {
      val testTask = (test in IntegrationTest).taskValue

      if (runImport.value) {
        val thyDir = (resourceDirectory in IntegrationTest).value.getPath

        // Use temporary task directory for dump
        val dumpDir = (taskTemporaryDirectory.value / "dump").getPath

        // Mount solr as resource - clean first
        val solrDir = (classDirectory in IntegrationTest).value / "solrdir"
        solrDir.delete()
        solrDir.mkdirs()

        // Run dump and dump_importer in Isabelle
        (run in isabelle)
          .toTask(" -A markup,theory -D " + thyDir + " -O " + dumpDir)
          .zip((run in `importer-isabelle`).toTask(" -L " + solrDir + " " + dumpDir))
          .flatMap { case (t1, t2) => t1 && t2 } && testTask
      } else {
        Def.task(testTask.value)
      }
    }.tag(Tags.Test).value,
    libraryDependencies ++= Seq(scalaTest % "it", classgraph % "it", scalaCompiler % "it", fastParse % "it")
  )
  .dependsOn(`common-utils` % "it", `common-dt` % "it", `importer-isabelle` % "it", `isabelle` % "it")

// Search application
// Core search module
lazy val `search-core` = project
  .configs(IntegrationTest)
  .settings(
    Defaults.itSettings,
    libraryDependencies ++= loggingBackend
      .map(_ % "it") ++ Seq(shapeless, circeGeneric, scalaTest % "it", scalaMock % "it")
  )
  .dependsOn(`common-dt`, `common-da-solr`, `common-utils`, `common-dt` % "it->it")

// Play web application backend
lazy val `search-webapp` = project
  .settings(
    javaOptions in Runtime ++= Seq(
      // Resource loading doesn't work properly in 'run' mode (only in prod), so we need to specify the logging conf here
      "-Dlog4j.configurationFile=" + (file("search-webapp") / "conf" / "log4j2.properties").getPath,
      "-Dsolr.configset=theorydata-" + schemaVersion
    ),
    libraryDependencies ++= (loggingBackend ++ circe ++ Seq(
      playGuice,
      playCirce,
      playSwagger,
      swaggerUi,
      playTestPlus % "test"
    )),
    dockerPermissionStrategy := DockerPermissionStrategy.Run,
    packageName in Docker := "findfacts",
    dockerBaseImage := "openjdk:11-jre-slim",
    dockerExposedPorts := Seq(9000),
    dockerRepository := Some("findfacts")
  )
  .enablePlugins(PlayScala)
  .disablePlugins(PlayLogback)
  .dependsOn(`search-core`, `search-webapp-ui`)

// Elm ui
lazy val `search-webapp-ui` = project
  .settings(publish / skip := true)
  .enablePlugins(ElmPlugin)

// Tools
lazy val `symbol-synonyms-tool` = project
  .settings(
    publish / skip := true,
    libraryDependencies ++= (loggingBackend ++ Seq(cmdOpts))
  )
  .dependsOn(`search-core`)

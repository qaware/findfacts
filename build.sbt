import Profiles._
import Dependencies._

Global / onChangedBuildSource := IgnoreSourceChanges

// Project-wide settings
ThisBuild / organization := "de.qaware.findfacts"
ThisBuild / version := "0.1.0-SNAPSHOT"
ThisBuild / scalaVersion := "2.12.10"
ThisBuild / resolvers ++= Resolvers.all
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
    `search-webapp`
  ))
  // Aggregate all modules
  .aggregate(
    `common-utils`,
    `common-da-api`,
    `common-da-solr`,
    `common-dt`,
    `importer-base`,
    loaders,
    `search-core`,
    `search-webapp`,
    ui
  )
  .enablePlugins(SonarConfiguration)

// Controls aggregation of subproject that need the elm-compiler (depending if ui profile is active)
lazy val ui = project
  .settings(
    publish / skip := true,
    aggregate := active(UiProfile)
  )
  .aggregate(`search-webapp`)

// Controls aggregation of subprojects that need isablelle (depending if loaders profile is active)
lazy val loaders = project
  .settings(
    publish / skip := true,
    aggregate := active(LoaderProfile)
  )
  .aggregate(`isabelle`, `importer-isabelle`)

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
    libraryDependencies ++= Seq(circeCore, solr, circeGeneric % "test") ++ loggingBackend.map(_ % "test")
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
    fork in run := true,
    javaOptions ++= Seq("-Xmx24G", "-Xss512m"),
    libraryDependencies ++= Seq(cmdOpts, cats),
  )
  .dependsOn(`common-dt`, `common-da-solr`, `common-utils`)

// Importer

// Isabelle project dependency
lazy val isabelle = project
  .settings(
    publish / skip := true,
    unmanagedJars in Compile ++= (baseDirectory.value / "lib" / "classes" ** "*.jar").get(),
    libraryDependencies ++= isabelleDependencies
  )

// Importer isabelle projects. Follows isabelle conventions.
lazy val `importer-isabelle` = project
  .settings(
    publish / skip := true,
    isabelleTool := "dump_importer",
    isabelleExecutable := (baseDirectory in isabelle).value / "bin" / "isabelle",
    libraryDependencies ++= loggingBackend
  )
  .dependsOn(`importer-base`, `isabelle`)
  .enablePlugins(IsabelleToolPlugin)

// Search application

// Core search module
lazy val `search-core` = project
  .configs(IntegrationTest)
  .settings(
    Defaults.itSettings,
    libraryDependencies ++= loggingBackend.map(_ % "it") ++ Seq(shapeless, circeGeneric, scalaTest % "it")
  )
  .dependsOn(`common-dt`, `common-da-solr`, `common-utils`, `common-dt` % "it->it")

// Play web application backend
lazy val `search-webapp` = project
  .settings(
    // Resource loading doesn't work properly in 'run' mode (only in prod), so we need to specify the logging conf here
    javaOptions in Runtime += "-Dlog4j.configurationFile=" + (file("`search-webapp`") / "conf" / "log4j2.properties").getPath,
    libraryDependencies ++= (loggingBackend ++ circe ++ Seq(
      playGuice, playCirce, playSwagger, swaggerUi, playTestPlus % "test"
    ))
  )
  .enablePlugins(PlayScala)
  .disablePlugins(PlayLogback)
  .dependsOn(`search-core`, `search-webapp-ui`)

// Elm ui
lazy val `search-webapp-ui` = project
  .settings(publish / skip := true)
  .enablePlugins(ElmPlugin)

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
ThisBuild / libraryDependencies ++= Seq(logging, wire, enum, files, scalaTest % "test")
// Don't run tests in assembly
ThisBuild / assembly / test := {}

// Root project aggregates all
lazy val root = (project in file("."))
  .settings(
    publish / skip := true,
    sonarProjects := Seq(
    `common-dt`,
    `common-solr`,
    `common-utils`,
    `search-core`,
    `search-webapp`,
    `importer-base`
  ))
  // Aggregate all modules
  .aggregate(
    `common-dt`,
    `common-solr`,
    `common-utils`,
    `search-core`,
    `search-webapp`,
    ui,
    `importer-base`,
    loaders,
    `yxml-parser`
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
lazy val `search-core` = project
  .configs(IntegrationTest)
  .settings(
    Defaults.itSettings,
    libraryDependencies ++= loggingBackend.map(_ % "it") ++ Seq(shapeless, circeGeneric, scalaTest % "it,test")
  )
  .dependsOn(`common-dt`, `common-solr`, `common-utils`)

// Play web application backend
lazy val `search-webapp` = project
  .settings(
    // Resource loading doesn't work properly in 'run' mode (only in prod), so we need to specify the logging conf here
    javaOptions in Runtime += "-Dlog4j.configurationFile=" + (file("`search-webapp`") / "conf" / "log4j2.properties").getPath,
    libraryDependencies ++= (loggingBackend ++ circe ++ Seq(
      playGuice, playCirce, playSwagger, swaggerUi, playTestPlus % "test"
    )),
  )
  .enablePlugins(PlayScala)
  .disablePlugins(PlayLogback, SbtElm)
  .dependsOn(`search-core`, `search-webapp-ui`)

// Elm ui
lazy val `search-webapp-ui` = project
  .settings(
    publish / skip := true,
    // Add elm sources to assets
    unmanagedSourceDirectories in Assets += baseDirectory.value / "src" / "elm",
  )
  .enablePlugins(SbtWeb)

// Common data types
lazy val `common-dt` = project
  .settings(libraryDependencies ++= Seq(shapeless, circeCore, playJson))
  .dependsOn(`common-utils`)

// Common solr entities and data access
lazy val `common-solr` = project
  .settings(libraryDependencies += solr)
  .dependsOn(`common-dt`, `common-utils`)

// Common utility
lazy val `common-utils` = project
  .settings(
    scapegoatIgnoredFiles += ".*scala.Using.scala",
    coverageExcludedFiles := "*.*scala.Using.scala",
  )

// Importer for isabelle dumps
lazy val `importer-base` = project
  .configs(IntegrationTest)
  .settings(
    fork in run := true,
    javaOptions ++= Seq("-Xmx24G", "-Xss512m"),
    libraryDependencies ++= Seq(cmdOpts, cats),
  )
  .dependsOn(`common-dt`, `common-solr`, `common-utils`)

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

// Parser for yxml
lazy val `yxml-parser` = project
  .settings(libraryDependencies ++= loggingBackend ++ Seq(cmdOpts, scalaParserCombinators, fastParse))

import Profiles._
import Dependencies._

// Project-wide settings
ThisBuild / organization := "de.qaware.findfacts"
ThisBuild / version := "0.1.0-SNAPSHOT"
ThisBuild / scalaVersion := "2.12.10"
ThisBuild / resolvers ++= Resolvers.all
// Parallel execution causes logging issues
ThisBuild / Test / parallelExecution := false
// Enable deprecation warnings
ThisBuild / scalacOptions += "-deprecation"
// Project-wide dependencies (intersection from all modules that can be run on their own)
ThisBuild / libraryDependencies ++= Seq(logging, enum, files, scalaTest % "test")
// Scapegoat plugin won't resolve without specifying version here
ThisBuild / scapegoatVersion := "1.3.8"

// Root project aggregates all
lazy val root = (project in file("."))
  .settings(sonarProjects := Seq(`common-dt`, `common-utils`, `common-solr`, core, webapp, `solr-dump-importer`))
  // Aggregate all modules
  .aggregate(core, ui, loaders, `yxml-parser`, `common-dt`, `common-utils`, `common-solr`)
  .enablePlugins(MultiProjectSonarPlugin)

// Controls aggregation of subproject that need the elm-compiler (depending if ui profile is active)
lazy val ui = project
  .settings(aggregate := active(UiProfile))
  .aggregate(`webapp`)

// Controls aggregation of subprojects that need isablelle (depending if loaders profile is active)
lazy val loaders = project
  .settings(aggregate := active(LoaderProfile))
  .aggregate(`solr-dump-importer`)

// Real sub-projects
// Importer for isabelle dumps
lazy val `solr-dump-importer` = project
  .configs(IntegrationTest)
  .settings(
    fork in run := true,
    javaOptions ++= Seq("-Xmx24G", "-Xss512m"),
    Defaults.itSettings,
    libraryDependencies ++= loggingBackend ++ Seq(cmdOpts, scalaCompiler % "it", scalaTest % "it"),
  )
  .dependsOn(`common-solr`, `common-dt`, `yxml-parser`, `common-utils`, `isabelle`)

// Isabelle project dependency
lazy val isabelle = project
  .settings(unmanagedJars in Compile ++= (baseDirectory.value / "lib" / "classes" ** "*.jar").get())

// Full-stack play web application, with elm ui
lazy val `webapp` = project
  .settings(
    // Resource loading doesn't work properly in 'run' mode (only in prod), so we need to specify the logging conf here
    javaOptions in Runtime += "-Dlog4j.configurationFile=" + (file("webapp") / "conf" / "log4j2.properties").getPath,
    libraryDependencies ++= (loggingBackend ++ circe ++ Seq(
      wire, playGuice, playCirce, playSwagger, swaggerUi, playTestPlus % "test"
    )),
  )
  .enablePlugins(PlayScala)
  .disablePlugins(PlayLogback, SbtElm)
  .dependsOn(core, `webapp-ui`)

lazy val `webapp-ui` = project
  .settings(
    // Add elm sources to assets
    unmanagedSourceDirectories in Assets += baseDirectory.value / "src" / "elm",
  )
  .enablePlugins(SbtWeb)


// Parser for yxml
lazy val `yxml-parser` = project
  .settings(libraryDependencies ++= loggingBackend ++ Seq(cmdOpts, scalaParserCombinators, fastParse))

lazy val core = project
  .configs(IntegrationTest)
  .settings(
    Defaults.itSettings,
    libraryDependencies ++= loggingBackend.map(_ % "it") ++ Seq(wire, shapeless, circeGeneric, scalaTest % "it,test")
  )
  .dependsOn(`common-dt`, `common-solr`, `common-utils`)

// Common data types
lazy val `common-dt` = project
  .settings(libraryDependencies ++= Seq(shapeless, circeCore, playJson))
  .dependsOn(`common-utils`)

// Common utility
lazy val `common-utils` = project
  .settings(
    scapegoatIgnoredFiles += ".*scala.Using.scala",
    coverageExcludedFiles := "*.*scala.Using.scala",
  )

// Common solr entities and data access
lazy val `common-solr` = project
  .settings(libraryDependencies += solr)
  .dependsOn(`common-dt`, `common-utils`)

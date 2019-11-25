// Project-wide settings
ThisBuild / organization := "de.qaware.findfacts"
ThisBuild / version := "0.1.0-SNAPSHOT"
ThisBuild / scalaVersion := "2.12.10"
// Parallel execution causes logging issues
ThisBuild / Test / parallelExecution := false
// Enable compiler optimizations
ThisBuild / scalacOptions ++= Seq(
  "-deprecation",
  "-opt:l:method",
  "-opt:l:inline",
  "-opt-inline-from:l:method,inline"
)
// Missing resolvers for restlet stuff
ThisBuild / resolvers += "Restlet" at "https://maven.restlet.com/"
// Settings for sonarqube integration
ThisBuild / scapegoatVersion := "1.3.8"
ThisBuild / scapegoatIgnoredFiles += ".*/Using.scala"
ThisBuild / coverageExcludedFiles := "*.*/Using.scala"
lazy val sonarSettings = Seq(
  sonarProperties ++= Map(
    "sonar.projectName" -> "Isabelle AFP Search",
    "sonar.projectKey" -> "de.qaware.isabelle-afp-search:root",
    "sonar.modules" -> "solr-dump-importer,yxml-parser,common-solr,common-utils",
    "sonar.junit.reportPaths" -> "target/test-reports",
    "sonar.scala.coverage.reportPaths" -> "target/scala-2.12/scoverage-report/scoverage.xml",
    "sonar.scala.scapegoat.reportPaths" -> "target/scala-2.12/scapegoat-report/scapegoat-scalastyle.xml",
    "sonar.scala.scalastyle.reportPaths" -> "target/scalastyle-result.xml"
  ))

// Project-wide dependencies
ThisBuild / libraryDependencies ++= Seq(
  scalatest % "test",
  "com.github.pathikrit" %% "better-files" % "3.8.0",
  "com.beachape" %% "enumeratum" % "1.5.13",
  "com.typesafe.scala-logging" %% "scala-logging" % "3.9.2",
  "org.apache.logging.log4j" % "log4j-slf4j-impl" % "2.12.1"
    exclude ("org.slf4j", "slf4j-api"),
  "org.apache.logging.log4j" % "log4j-core" % "2.12.1"
)
// Named dependencies
val scalatest = "org.scalatest" %% "scalatest" % "3.0.8"
val scopt = "com.github.scopt" %% "scopt" % "3.7.1"


// Project structure: root project for common settings and to aggregate tasks
lazy val root = (project in file("."))
  .settings(sonarSettings)
  .aggregate(`solr-dump-importer`, `common-solr`, `common-utils`, `yxml-parser`, `webapp`)
  .settings(aggregate in sonarScan := false)

// Importer for isabelle dumps
lazy val `solr-dump-importer` = project
  .configs(IntegrationTest)
  .settings(
    fork in run := true,
    javaOptions ++= Seq("-Xmx24G", "-Xss512m"),
    inConfig(IntegrationTest)(Defaults.testSettings),
    Defaults.itSettings,
    libraryDependencies ++= Seq(
      scopt,
      scalatest % "it",
      "org.scala-lang" % "scala-compiler" % "2.12.10" % "it"
    ),
  )
  .dependsOn(`common-solr`, `yxml-parser`, `common-utils`)

// Common solr entities and data access
lazy val `common-solr` = project
  .settings(
    libraryDependencies += "org.apache.solr" % "solr-core" % "8.2.0"
      exclude ("org.slf4j", "slf4j-api")
      exclude ("org.apache.logging.log4j", "log4j-api")
      exclude ("org.apache.logging.log4j", "log4j-core")
      exclude ("org.apache.logging.log4j", "log4j-slf4j-impl")
  )
  .dependsOn(`common-utils`)

// Parser for yxml
lazy val `yxml-parser` = project
  .settings(libraryDependencies ++= Seq(
    scopt,
    "org.scala-lang.modules" %% "scala-parser-combinators" % "1.1.2",
    "com.lihaoyi" %% "fastparse" % "2.1.3"
  ))

// Common utility
lazy val `common-utils` = project

// Full-stack play web application, with elm ui
lazy val `webapp` = project
  .settings(
    // Resource loading doesn't work properly in 'run' mode (only in prod), so we need to specify the logging conf here
    javaOptions in Runtime += "-Dlog4j.configurationFile=webapp/conf/log4j2.properties",
    // Add elm sources to assets
    unmanagedSourceDirectories in Assets += baseDirectory.value / "elm/src",
    // Build elm compiler (npm module) before elmMake, and configure SbtElm to use built elm compiler.
    (Assets / ElmKeys.elmMake) := (Assets / ElmKeys.elmMake).dependsOn(elm / Compile / npmInstallDependencies).value,
    ElmKeys.elmExecutable in ElmKeys.elmMake in Assets := (baseDirectory.value /
      "elm/target/scala-2.12/scalajs-bundler/main/node_modules/.bin/elm").getAbsolutePath + " make",
    libraryDependencies ++= Seq(
      guice exclude ("org.slf4j", "slf4j-api"),
      "org.scalatestplus.play" %% "scalatestplus-play" % "4.0.3" % Test exclude ("org.slf4j", "slf4j-api"),
      "io.swagger" %% "swagger-play2" % "1.7.1"
        exclude("com.google.guava", "guava")
        exclude("com.typesafe.play", "play-logback_2.12"),
      "org.webjars" % "swagger-ui" % "3.24.3"
    ),
  )
  .enablePlugins(PlayScala)
  .disablePlugins(PlayLogback)
  .dependsOn(`common-solr`)

// ScalaJS project to wrap elm npm dependencies.
// Can't be part of 'webapp' as PlayScala and ScalaJSBundler are incompatible.
lazy val elm = (project in file("webapp/elm"))
  .settings(npmDevDependencies in Compile ++= Seq(
    "elm" -> "^0.19",
    "elm-format" -> "^0.8"
  ))
  .enablePlugins(ScalaJSBundlerPlugin)

ThisBuild / organization := "de.qaware"
ThisBuild / version := "0.1.0-SNAPSHOT"
ThisBuild / scalaVersion := "2.12.10"
ThisBuild / scapegoatVersion := "1.3.8"

// Enable compiler optimizations
ThisBuild / scalacOptions ++= Seq(
  "-deprecation",
  "-opt:l:method",
  "-opt:l:inline",
  "-opt-inline-from:l:method,inline"
)

// Project-wide dependency management
ThisBuild / resolvers += "Restlet" at "https://maven.restlet.com/"
ThisBuild / libraryDependencies ++= Seq(
  "com.github.pathikrit" %% "better-files" % "3.8.0",
  "org.scalatest" %% "scalatest" % "3.0.8" % "test",
  "com.typesafe.scala-logging" %% "scala-logging" % "3.9.2",
  "org.apache.logging.log4j" % "log4j-slf4j-impl" % "2.12.1" exclude ("org.slf4j", "slf4j-api"),
  "org.apache.logging.log4j" % "log4j-core" % "2.12.1"
)

lazy val root = (project in file("."))
  .settings(sonarSettings)
  .aggregate(`solr-dump-importer`, `common-solr`, `common-utils`, `yxml-parser`)
  .settings(aggregate in sonarScan := false)

lazy val `solr-dump-importer` = project
  .settings(libraryDependencies += "com.github.scopt" %% "scopt" % "3.7.1")
  .dependsOn(`common-solr`, `yxml-parser`, `common-utils`)

lazy val `common-solr` = project
  .settings(
    libraryDependencies += "org.apache.solr" % "solr-core" % "8.2.0"
      exclude ("org.slf4j", "slf4j-api")
      exclude ("org.apache.logging.log4j", "log4j-api")
      exclude ("org.apache.logging.log4j", "log4j-core")
      exclude ("org.apache.logging.log4j", "log4j-slf4j-impl")
  )
  .dependsOn(`common-utils`)

lazy val `yxml-parser` = project
  .settings(libraryDependencies += "org.scala-lang.modules" %% "scala-parser-combinators" % "1.1.2")

lazy val `common-utils` = project

// Settings for sonarqube integration
ThisBuild / coverageExcludedFiles := "*.*/Using.scala"
ThisBuild / scapegoatIgnoredFiles += ".*/Using.scala"

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
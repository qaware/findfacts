ThisBuild / organization := "de.qaware"
ThisBuild / version := "0.1.0-SNAPSHOT"
ThisBuild / scalaVersion := "2.13.1"

// Project-wide dependency management
ThisBuild / resolvers += "Restlet" at "https://maven.restlet.com/"
ThisBuild / libraryDependencies ++= Seq(
  "com.github.pathikrit" %% "better-files" % "3.8.0",
  "org.scalatest" %% "scalatest" % "3.0.8" % "test",
  "com.typesafe.scala-logging" %% "scala-logging" % "3.9.2",
  "org.apache.logging.log4j" % "log4j-slf4j-impl" % "2.12.1" exclude("org.slf4j", "slf4j-api"),
  "org.apache.logging.log4j" % "log4j-core" % "2.12.1"
)

lazy val root = (project in file("."))
  .settings()
  .aggregate(importer, solr, yxml)

lazy val importer = (project in file("solr-dump-importer"))
  .settings(
    libraryDependencies += "com.github.scopt" %% "scopt" % "3.7.1",
  )
  .dependsOn(solr, yxml)

lazy val solr = (project in file("common-solr"))
  .settings(
    libraryDependencies += "org.apache.solr" % "solr-core" % "8.2.0"
      exclude("org.slf4j", "slf4j-api")
      exclude("org.apache.logging.log4j", "log4j-api")
      exclude("org.apache.logging.log4j", "log4j-core")
      exclude("org.apache.logging.log4j", "log4j-slf4j-impl")
  )

lazy val yxml = (project in file("yxml-parser"))
  .settings(
    libraryDependencies += "org.scala-lang.modules" %% "scala-parser-combinators" % "1.1.2"
  )
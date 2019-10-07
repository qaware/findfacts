ThisBuild / organization := "de.qaware"
ThisBuild / version := "0.1.0-SNAPSHOT"
ThisBuild / scalaVersion := "2.13.1"

ThisBuild / resolvers += "Restlet" at "https://maven.restlet.com/"

lazy val root = (project in file("."))
  .settings()
  .aggregate(importer, solr)

lazy val importer = (project in file("solr-dump-importer"))
  .settings(
    libraryDependencies ++= Seq(
      "com.github.scopt" %% "scopt" % "3.7.1",
    )
  )
  .dependsOn(solr)

lazy val solr = (project in file("common-solr"))
  .settings(
    libraryDependencies ++= Seq(
      "org.apache.solr" % "solr-core" % "8.2.0",
    )
  )
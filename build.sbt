ThisBuild / organization := "de.qaware"
ThisBuild / version := "0.1.0-SNAPSHOT"
ThisBuild / scalaVersion := "2.13.1"

lazy val root = (project in file("."))
  .settings()
  .aggregate(importer)

lazy val importer = (project in file("solr-dump-importer"))
  .settings()
